# Upstream PR draft: isocline modifyOtherKeys

Target: https://github.com/daanx/isocline. Not yet submitted.
Source: the `tty_esc.c` hunk of `vendor/isocline.patch`. Leave the `isocline.cabal` and `test/` hunks out of the PR; they are local to hilda.
Unverified: upstream's default branch, and whether an open issue or PR already covers this.

---

## Title

Parse xterm modifyOtherKeys (`ESC [ 27 ; mods ; key ~`), so Shift+Enter works

## Body

kitty, WezTerm and Ghostty send Shift+Enter as the xterm modifyOtherKeys sequence:

```
ESC [ 27 ; 2 ; 13 ~
```

`tty_read_esc` in `src/tty_esc.c` reads at most two CSI parameters. It stops at the second `;`, so the sequence does not decode. The remaining `13~` is inserted into the line as text.

This patch reads a third parameter when the first is `27`. If the final byte is `~`, it rewrites the sequence as the equivalent CSI u form, `ESC [ <key> ; <mods> u`. The existing CSI u path then decodes it. Shift+Enter becomes `KEY_LINEFEED`, as `tty.h` already intends ("ctrl/shift + enter is considered KEY_LINEFEED").

The change only affects sequences that start `ESC [ 27 ;` and have a third parameter. All other sequences parse as before, including `ESC [ 27 ~` and `ESC [ 27 ; <mods> ~`.

```diff
--- a/src/tty_esc.c
+++ b/src/tty_esc.c
@@ -265,6 +265,14 @@
     if (!tty_readc_noblock(tty,&peek,esc_timeout)) return KEY_NONE;
     tty_read_csi_num(tty,&peek,&num2,esc_timeout);
   }
+  // xterm modifyOtherKeys: ESC [ 27 ; <mods> ; <key> ~  (e.g. shift+enter);
+  // read it as ESC [ <key> ; <mods> u
+  if (peek == ';' && c1 == '[' && num1 == 27) {
+    uint32_t num3 = 1;
+    if (!tty_readc_noblock(tty,&peek,esc_timeout)) return KEY_NONE;
+    tty_read_csi_num(tty,&peek,&num3,esc_timeout);
+    if (peek == '~') { num1 = num3; peek = 'u'; }
+  }
 
   // the final character (we do not allow 'intermediate characters')
   uint8_t final = peek;
```

### Testing

Tested with the Haskell binding (isocline 1.1.0 from Hackage), using Shift+Enter at a multi-line prompt:

- Before: `13~` appears in the input.
- After: a newline is inserted.

If accepted, a Hackage release (1.1.x) of the Haskell package would let downstream users drop their vendored copies.

Reference: xterm `modifyOtherKeys`, https://invisible-island.net/xterm/modified-keys.html
