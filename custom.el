(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :weight normal :height 110 :width normal :family "Monoid"))))
 '(elscreen-tab-background-face ((t (:inherit default))))
 '(elscreen-tab-current-screen-face ((t (:height 0.8))))
 '(elscreen-tab-other-screen-face ((t (:inherit linum))))
 '(fringe ((t (:inherit default :background nil))))
 '(linum ((t (:inherit default :box nil :height 0.8 :background nil))))
 '(magit-item-highlight ((t nil)))
 '(mode-line-inactive ((t (:inherit default :background nil))))
 '(org-block-background ((t (:background "#070707"))))
 '(org-block-begin-line ((t (:background "#002D43"))))
 '(org-block-end-line ((t (:background "#002D43"))))
 '(powerline-active1 ((t (:inherit default :background nil))))
 '(powerline-active2 ((t (:inherit default :background nil))))
 '(powerline-inactive1 ((t (:inherit default :background nil))))
 '(powerline-inactive2 ((t (:inherit default :background nil)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "c006bc787154c31d5c75e93a54657b4421e0b1a62516644bd25d954239bc9933" "ad24ea739f229477ea348af968634cb7a0748c9015110a777c8effeddfa920f5" "ffe39e540469ef05808ab4b75055cc81266875fa4a0d9e89c2fec1da7a6354f3" "33bb2c9b6e965f9c3366c57f8d08a94152954d4e2124dc621953f5a8d7e9ca41" "12722541c8998f056b761bf63a92216aaf4610e4eb1afe7991842a31fa28b6d8" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "764e3a6472a3a4821d929cdbd786e759fab6ef6c2081884fca45f1e1e3077d1d" "fc58296b4831a327aa1b11939b32755a7bd4a0679665809968132ef38d2dd4d8" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "acb039d6f2c41b3bd852b448351b2979f44ef488026c95dd5228d2f6da57f574" "36b22a0d7e56042cda5c6a3aa589adfcde0660f3a048feaa54c2797cfb090510" "e4bc8563d7651b2fed20402fe37b7ab7cb72869f92a3e705907aaecc706117b5" "ddfb81ce0b4bbd47433866401713fcca73db8eb0d50bd323bad0f052155c995c" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "5ea20171762b3f9682fbf507ee4b4018ce7b6cc65415fa99799a125f112b2cdb" "30a8a5a9099e000f5d4dbfb2d6706e0a94d56620320ce1071eede5481f77d312" "8d6fb24169d94df45422617a1dfabf15ca42a97d594d28b3584dc6db711e0e0b" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" "08efabe5a8f3827508634a3ceed33fa06b9daeef9c70a24218b70494acdf7855" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "1a85b8ade3d7cf76897b338ff3b20409cb5a5fbed4e45c6f38c98eee7b025ad4" "0e7da2c7c64fb5d4764250ffa4b8b33c0946577108d1d6444f1020d0dabba784" "aea04a6cf90e3ad60e0aeb199d3e2ad06ab20d71150d5870dcb7117b6c554b0a" "a5cd126cd6dcf911a3f57c7e5d8ec652763cadcdcd317150295483454167227e" "a0ffdee5e0d310b62da5223f76e70d7b2ad122943e0bb90ec604e7394c1ec77e" "d1c2ea65f9163ea61908deba09f321640ed8733346bb1cb388eabf0310ccaf89" "fa942713c74b5ad27893e72ed8dccf791c9d39e5e7336e52d76e7125bfa51d4c" "57f8801351e8b7677923c9fe547f7e19f38c99b80d68c34da6fa9b94dc6d3297" "bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" "b6f7795c2fbf75baf3419c60ef7625154c046fc2b10e3fdd188e5757e08ac0ec" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "77d704908bf206929237af1089f020781963dcc671489005fb8153c1745f689a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "aa392ee7deac22c0c0b71b396d4969ba0849b538bb1790ef31b115b4e620c0b5" "9c26d896b2668f212f39f5b0206c5e3f0ac301611ced8a6f74afe4ee9c7e6311" "36d0f600074e9299fb7b6a316161d99faa16a6551ddeda50980ae293e653e7b4" default)))
 '(fci-rule-color "#49483E")
 '(fringe-mode nil nil (fringe))
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   (("#49483E" . 0)
    ("#67930F" . 20)
    ("#349B8D" . 30)
    ("#21889B" . 50)
    ("#968B26" . 60)
    ("#A45E0A" . 70)
    ("#A41F99" . 85)
    ("#49483E" . 100)))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(linum-format " %d ")
 '(linum-relative-current-symbol "")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(powerline-height 10)
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#A1EFE4" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#F92672" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#A6E22E"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#66D9EF" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#E6DB74"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#FD5FF0"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#FD971F" :weight bold))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
