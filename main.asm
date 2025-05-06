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

main	lda	#0		;
-	pha			;
	pha			;

	asl			;
	asl			;
	clc			;
	adc	#8		;
	tax			;

	pla			;
	lsr			;
	ldy	#$17		;
	jsr	cardsho		;

	pla			;
	clc			;
	adc	#$01		;
	and	#$07		;
	bne	-		;
-	jsr	$ffe4
	beq	-
	rts
	
cardsho	pha			;void cardsho(register uint8_t& a,
	cpx	#SCREENW	;              register uint8_t& x,
	bcs	+++		;              register uint8_t& y) {
	lda	#<SCREENM	; if (x < SCREENW) {
	sta	selfsha		; 
	lda	#>SCREENM	;
	sta	1+selfsha	;  selfsha = SCREENM;
	cpy	#SCREENH-1	;
	bcc	+		;  if (y >= SCREENH - 1)
	pla			;
	jmp	cardout		;   return; // not allowed to draw even one line
+	tya			;
	beq	+		;
-	clc			;
	lda	#SCREENW	;
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
	sta	1+selfsha	;  selfsha = y * SCREENW + x;
	clc			;
	.if <+SCREEND
	lda	selfsha		;
	adc	#<+SCREEND	;
	sta	selfcla		;
	lda	1+selfsha	;
	.endif
	adc	#>+SCREEND	;
	sta	1+selfcla	;
	.if !<+SCREEND		;
	lda	selfsha		;
	sta	selfcla		;  selfcla = 0xffff & (selfsha + SCREEND);
	.endif

+	pla			; } // else skip the selfmod, using last value
	tax			; x = a; // card code, no mask (>= 8 "not card")
	clc			;
	adc	#$f8		;
	bcs	blankit		; if (a < 0x08) { // so can index into cardnum[]

	and	#$07		;
	tay			;  y = a & 0x07; // card code, masked onto 0 ~ 7
	clc			;
	adc	#$fc		;  uint1_t c = (a & 0x04) ? 1 : 0; // bit 2 to c

	lda	cardnum,y	;  a = cardnum[y];
	ldy	#0		;
	jsr	selfsho		;  selfsho(cardnum[y], y = 0); // 1 ~ 4, A ~ D

	bcc	+		;
	lda	threatc,y	;
	bcs	++		;
+	lda	investc,y	;
+	iny			;
	jsr	selfsho		;  selfsho(c ? threatc[0] : investc[0], y = 1);

	bcc	+		;
	lda	threatc,y	;
	bcs	++		;
+	lda	investc,y	;
+	iny			;
	jsr	selfsho		;  selfsho(c ? threatc[1] : investc[1], y = 2);

blankit	ldy	#0		; } // top of a bona fide card has been drawn
	lda	1+selfsha	; y = 0;
	cmp	#>ONLYTOP	;
	bcs	+		;
	bne	++		;
	lda	selfsha		;
	cmp	#<ONLYTOP	;
	bcc	++		; if (selfsha >= ONLYTOP) {
+	cpx	#8		;  if (x >= 8)
	bcs	cardtop		;   goto cardtop; // draw the blank card
	ldy	#2		;  else
	bcc	colrtop		;   goto colrtop; // only finish the color info

+	ldy	#SCREENW*5	; }
-	tya			; for (y = SCREENW*5; y; ) {
	sec			;
	sbc	#SCREENW	;
	tay			;  y -= SCREENW; // SCREENW*4 down to 0 (or *1!)
	lda	#$a0		;  cardtop:
	cpx	#8		;
	bcc	+		;
cardtop	lda	#$20		;  a = (x<8) ? 0xa0 /*solid*/ : 0x20 /*blank*/;
+	jsr	selfsho		;  selfsho(a, y);
	iny			;
	jsr	selfsho		;  selfsho(a, ++y);
	iny			;
	jsr	selfsho		;  selfsho(a, ++y);
	cpx	#8		;
	bcc	colrtop		;  if (x<8) {
	cpy	#0		;
	bne	-		;
colrtop	lda	cardclr,x	;   a = cardclr[x];
	jsr	selfclr		;   selfclr(a, y);
	dey			;	
	jsr	selfclr		;   selfclr(a, --y);
	dey			;	
	jsr	selfclr		;   selfclr(a, --y);
	beq	cardout		;   if (y == 0 || y <= SCREENW) break;
	cpy	#SCREENW+1	;  }   // express exit, don't overwrite top    
	bcc	cardout		; }
cardout	rts			;} // cardsho()

selfsho	.byte	$99		;static uint8_t* selfsha = SCREENM;
selfsha	.byte	<SCREENM	;void selfsho(uint8_t a, uint8_t y) {
	.byte	>SCREENM	; selfsha[y] = a; // sta $XXXX,y
	rts			;} // selfsho()

selfclr	.byte	$99		;static uint8_t* selfclr = SCREENC;
selfcla	.byte	<SCREENC	;void selfclr(uint8_t a, uint8_t y) {
	.byte	>SCREENC	; selfcla[y] = a; // sta $XXXX,y
	rts			;} // selfclr()






vararea
pre_end
.end
