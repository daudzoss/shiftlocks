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
	.text	$94,"'"
	.null	$11,$11,$11,$11,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                 "
	.null	$ce,$22
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
	.null	$ce,$22
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
	.text	" of  ",$c0,$d7
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
	
SCREEND	= SCREENC-SCREENM
SCRSIZE	= SCREENW*SCREENH
ONLYTOP	= SCREENM+SCREENW*(SCREENH-8)

stackht	= vararea + $0		; 0-3 invest, 4-7 threat, 8-11 office
;????	= vararea + $0c
	
start

;;; 1,2,3,4 = 000,001,010,011
;;; A,B,C,D = 100,101,110,111
cardnum
invest0	.text	$b1,$b2,$b3,$b4	; const
threat0	.text	$81,$82,$83,$84
cardtyp
investc	.text	$c0,$d7
threatc	.text	$db,$c0
cardclr	.text	$62,$63,$64,$65
	.text	$66,$67,$68,$69
	
main
-	jsr	$ffe4
	beq	-
	ldx	#$08		;
	ldy	#$17		;
	lda	#3		;
	jsr	cardsho		;
-	jsr	$ffe4
	beq	-
	rts
	
	
cardsho	pha			;
	cpx	#SCREENW	;
	bcs	++		; just skip the selfmod, use last value
	lda	#<SCREENM	;
	sta	selfsha		;
	lda	#>SCREENM	;
	sta	1+selfsha	;
	tya			;
	beq	+		;
	cpy	#SCREENH-1	;
	bcc	+		;
	pla			;
	jmp	cardout		; return;
+	lda	#SCREENW	;
	;clc			;
	adc	selfsha		;
	sta	selfsha		;
	lda	#0		;
	adc	1+selfsha	;
	sta	1+selfsha	;
	dey			;
	bne	-		;
+	txa			;
	clc			;
	adc	selfsha		;
	sta	selfsha		;
	lda	#0		;
	adc	1+selfsha	;
	sta	1+selfsha	;
	clc			;
	lda	selfsha		;
	adc	#<+SCREEND	;
	sta	selfcla		;
	lda	1+selfcla	;
	adc	#0		;
	sta	1+selfcla	;
	

+	pla			;
	clc			;
	adc	#$f8		; c = (a & 0x04) ? 1 : 0; // move bit 2 to carry
	tax			;
	and	#$07		; a &= 07;
	tay			;

	lda	cardnum,y	;
	ldy	#0		;
	jsr	selfsho		;
	
	bcs	+		;
	lda	investc,y	;
	bcc	++		;
+	lda	threatc,y	;
+	iny			;
	jsr	selfsho		;
	
	bcs	+		;
	lda	investc,y	;
	bcc	++		;
+	lda	threatc,y	;
+	iny			;
	jsr	selfsho		;
	
	ldy	#2		;
	lda	1+selfsha	;
	cmp	#>ONLYTOP	;
	bcs	cardtop		;
	bne	+		;
	lda	selfsha		;
	cmp	#<ONLYTOP	;
	bcs	cardtop		;
	
+	ldy	#SCREENW*5	;
-	tya			;
	sec			;
	sbc	#SCREENW	;
	tay			;
	lda	#$a0		;
	cpx	#8		;
	bcc	+		;
blankit	lda	#$20		;
+	jsr	selfsho		;
	iny			;
	jsr	selfsho		;
	iny			;
	jsr	selfsho		;
cardtop	lda	cardclr,x	;
	jsr	selfclr		;
	dey			;	
	jsr	selfclr		;
	dey			;	
	jsr	selfclr		;
	cpy	#0		;
	bne	-		;
cardout	rts			;

selfsho	.byte	$99		;static uint8_t* selfsha = SCREENM;
selfsha	.byte	<SCREENM	;void selfsho(uint8_t a, uint8_t y) {
	.byte	>SCREENM	; selfsha[y] = a; // sta $XXXX,y
	rts			;} // selfsho()

selfclr	.byte	$99		;
selfcla	.byte	<SCREENC	;
	.byte	>SCREENC	;
	rts			;






vararea
pre_end
.end
