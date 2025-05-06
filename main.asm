.if BASIC
*	= BASIC+1
.else
*	= $0002+1
COPIED2	= $0400
	.word	(+), 3
	.text	$81,$41,$b2,$30	; FOR A = 0
	.text	$a4		; TO prefld-start
	.text	format("%4d",pre_end-start)
	.text	$3a,$dc,$30	; : BANK 0
	.text	$3a,$42,$b2,$c2	; : B = PEEK
	.text	$28		; ( start
	.text	format("%2d",COPIED2)
	.text	$aa,$41,$29,$3a	; + A ) :
	.text	$dc,$31,$35,$3a	; BANK 1 5 :
	.text	$97		; POKE start
	.text 	format("%2d",COPIED2)
	.text	$aa,$41,$2c,$42	; + A , B
	.text	$3a,$82,$00	; : NEXT
+
.endif

	.word	(+), 2055
	.text	$99,$22,$09,$8e	; PRINT " CHR$(9) CHR$(142)
	.text	$08,$93,$13,$13	; CHR$(8) CHR$(147) CHR$(19) CHR$(19)
topline	.text	"the crime scene     "
	.text	"  shiftlocks office"
	.text	$9d,$9d,$9d,$9d
	.text	$9d,$9d,$9d,$9d
	.text	$94,"'",$22
	.text	$3a,$99,$3a,$99
	.null	$3a,$99,$3a,$99
+	.word	(+), 2055
	.text	$99,$22
	.text	"                 "
.if BASIC
	.null	$6e,$22
.else
	.null	"/",$22
.endif
+	.word	(+), 2055
	.text	$99,$22
	.text	"    threats      "
	.null	"dis-",$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"   ",$12,"wounds: 0"
	.null	$92,"     card",$22
+	.word	(+), 2055
	.text	$99,$22
	.text	" investigations  "
	.null	$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                 "
	.null	"pile",$22,$3a,$99
+	.word	(+), 2055
	.text	$99,$22,$11
	.text	"                 "
	.null	" draw",$22
+	.word	(+), 2055	
	.text	$99,$22,$11
	.text	"                 "
	.null	" deck",$22
+	.word	(+), 2055
	.text	$99,$22
	.text	" ",$5e,"f1"
	.text	" ",$5e,"f3"
	.text	" ",$5e,"f5"
	.text	" ",$5e,"f7"
	.text	"     "
.if BASIC
	.null	$6e,$22
.else
	.null	"/",$22
.endif
+	.word	(+), 2055
	.text	$99,$22
	.text	"                  "
	.null	$12,"   ",$92,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                  "
	.null	$12,"   ",$92,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                  "
	.null	$12," ? ",$92,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                  "
	.null	$12,"   ",$92,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                  "
	.null	$12,"   ",$92,$22
+	.word	(+),2055
	.text	$99,$22
	.text	" f2",$c0," f4",$c0
	.text	" f6",$c0," f8",$c0
	.text	" to solve: 3 col"
	.text	" of  ",$c0,$77
	.text	$9d,$9d,$9d,$94
	.null	$34,$22,$3a,$99
+	.word	(+),2055	
	.text	$99,$22,"pairing "
	.text	"   +       +    "
	.null	"   +       +",$22
+	.word	(+),2055
	.text	$99,$22
	.text	" rules: invest",$c0
	.text	" ",$5f,"threat"
	.text	" ",$5f,"invest"
	.null	" ex pile",$22,$3b
+	.word	(+),2055
	.text	$9e
	.null	format("%4d",main)
+	.word	0

.if !BASIC
*	= COPIED2
.endif
	
start

main
	jsr	$ffe4
	beq	main
	rts
pre_end
