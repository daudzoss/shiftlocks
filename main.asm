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

DECKSIZ	= pstdeck-stddeck
STACKHT	= vararea + $00		; 0-3 invest, 4-7 threat, 8-11 office
HAND	= vararea + $0c		;
DISCARD	= vararea + $10
DECK	= DISCARD +DECKSIZ
HANDREM = DISCARD +2*DECKSIZ	; should always be 0 or 4?
DISCREM	= DISCARD +2*DECKSIZ+ 1
DECKREM	= DISCARD + DECKSIZ + 2
HANDFOM	= DISCARD + DECKSIZ + 3
HANDGET	= DISCARD + DECKSIZ + 4
;????	= DISCARD + DECKSIZ + 5
TEMPVAR	= DISCARD + DECKSIZ + $f

start

stddeck	.text	$00,$00,$00,$00,$00,$00,$00,$00
	.text	$01,$01,$01,$01,$01,$01,$01,$01
	.text	$02,$02,$02,$02,$02,$02,$02,$02
	.text	$03,$03,$03,$03,$03,$03,$03,$03
	.text	$04,$04,$04,$04,$05,$05,$05,$05
	.text	$06,$06,$06,$06,$07,$07,$07,$07
pstdeck


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
drawx	.text	$11
drawy	.text	$05
discx	.text	$12
discy	.text	$0b
stackx	.text	$00,$04,$08,$0c
	.text	$00,$04,$08,$0c	
	.text	$16,$1b,$20,$25
stacky	.text	9,9,9,9,1,1,1,1
	.text	1,1,1,1		;bottom card of a stack allowed to grow up to...
stacklm	.text	1,1,1,1,1,1,1,1
	.text	$10,$10,$10,$10	;16 (alternating invest with threat or removals)

main	lda	#0		;void main (void) {
	sta	DISCREM		;
	sta	HANDREM		; DISCREM = HANDREM = 0;
	jsr	finishr		; finishr(); // rule text completed using colors
	jsr	initstk		; initstk();
	lda	#DECKSIZ	;
-	jsr	shuffle		; shuffle(/* DECKREM =*/ DECKSIZ);
	jsr	drw4hnd		; 
	sta	HANDFOM		; HANDFOM = drw4hand(); // nonzero if we drew 4

;	bne	+		;  if (HANDFOM)
;	jsr	drewall		;   break;
;	and	#$fc		;  if (drewall() >= 4) // also the new DECKREM

	bne	-		; if (HANDFOM == 0)
	brk			;  break; // more than 44 cards in office?!?
+	jsr	animhnd		; animhnd(); // draw empty deck pile after, if 0


-	jsr	$ffe4		;
	beq	-		; getchar();
	rts			;} // main()

finishr	lda	#0		;void finishr(void) {
-	pha			; register uint8_t a, x, y;
	pha			; for (a = 0; a < 8; a++) {
	asl			;
	asl			;
	clc			;
	adc	#8		;
	tax			;  x = 4 * a + 8; // cols 8, 12, ..., 36
	pla			;
	lsr			;
	ldy	#$17		;  y = 23;
	jsr	cardsho		;  cardsho(a >> 2, x, y); // a = 1,1,2,2,3,3,4,4
	pla			;
	clc			;
	adc	#$01		;
	and	#$07		;
	bne	-		; }
	rts			;} // finishr()

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

initstk	lda	#$00		;void initstk(void) {
	ldx	#$0c		; for (register uint8_t x = 11; x >= 0; x--)
-	sta	STACKHT-1,x	;
	dex			;  STACKHT[x] = 0;
	bne	-		; }
	rts			;} // initstk()

shuffle	ldy	#$ff		;void shuffle(register uint8_t& a) {
	tax			;
	beq	shuffl1		; if (a > 0) { // # cards to copy from stddeck
	sta	DECKREM		;  DECKREM = a;
-	lda	stddeck-1,x	;  for (register int8_t x = a-1; x >= 0; x--) {
	sta	DECK-1,x	;   DECK[x] = stddeck[x];
	dex			;  }
	bne	-		; }
shuffl1	ldx	#$ff		; for (register uint8_t y = 255; y; y--) {
shuffl2	txa			;  for (register uint8_t x = 255; x; x--) {
	pha			;
	tya			;   register uint8_t x, y; // local
	pha			;
	lda	RNDLOC1		;   for (x = (*RNDLOC1 ^ *RNDLOC2) >> 1;
	eor	RNDLOC2		;        x >= *DECKREM;
-	lsr			;        x >>= 1)
	cmp	DECKREM		;
	bcs	-		;    ; // x now a valid index into the deck
	tax			;
	lda	RNDLOC1		;   for (y = (*RNDLOC1 ^ *RNDLOC2) >> 1;
	eor	RNDLOC2		;        y >= *DECKREM;
-	lsr			;        y >>= 1)
	cmp	DECKREM		;
	bcs	-		;    ; // y now a valid index into the deck
	tay			;
	lda	DECK,x		;
	pha			;   uint8_t temp = DECK[x];
	lda	DECK,y		;
	sta	DECK,x		;   DECK[x] = DECK[y];
	pla			;
	sta	DECK,y		;   DECK[y] = temp;
	pla			;
	tax			;
	pla			;
	tay			;
	dey			;
	bne	shuffl2		;  }
	dex			;
	bne	shuffl1		; }
	rts			;} //shuffle()

drw1new	ldy	DECKREM		;int8_t drw1new(void) {
	dey			;
	bmi	+		; if (DECKREM > 0 /* || DECKREM == -128*/ )
	dey			;
	sty	DECKREM		;
	lda	DECK,y		;  return DECK[--DECKREM];
	rts			; else
+	lda	#$ff		;  return -1;
	rts			;} // drw1new()

drwlnyb	.text	$01,$02,$04,$08	;static const uint8_t drwlnyb[] = {1, 2, 4, 8};
drwhnyb	.text	$10,$20,$40,$80	;static const uint8_t drwhnyb[] = {16,32,64,128,
	.text	$10,$20,$40,$80	;                                 16,32,64,128};
drw4hnd	lda	HANDREM		;void drw4hand(void) {
	bne	+++		; if (HANDREM == 0) { // should've emptied first
	ldx	#4		;  uint8_t retval = 0; // return figure of merit
-	pha			;  for (register uint8_t x = 3; x >= 0; x--) {
	jsr	drw1new		;   register uint8_t a = drw1new();
	bpl	+		;   if (a < 0) { // deck ran out
	txa			;
	pha			;
	ldy	DISCREM		;    register int8_t y = DISCREM;
	sty	DECKREM		;
-	lda	DISCARD-1,y	;    for (DECKREM = y--; y >= 0 ; y--) {
	sta	DECK-1,y	;     DECK[y] = DISCARD[y];
	dey			;
	bne	-		;    }
	sty	DISCREM		;    DISCREM = 0;
	jsr	shuffle		;    shuffle();
	pla			;
	tax			;
	jsr	drw1new		;    a = drw1new();
	bpl	+		;    if (a < 0) // all cards evaporated somehow?
	brk			;     exit(1);
	.text	1		;   } // a is a valid card in the range 0 ~ 7
+	;and	#$07		;
	sta	HAND-1,x	;   HAND[x] = a /* & 0x07 */;
	tay			;
	pla			;
	cpy	#4		;
	bcc	+		;   if (HAND[x] >= 4)//(HAND[x]>=4)?1<<x:0;//T:I
	ora	drwlnyb-1,x	;    retval |= drwlnyb[x];
+	ora	drwhnyb,y	;   retval |= drwhnyb[HAND[x]]; // nybbles valid
	dex			;  }
	bne	--		;  return retval;
	rts			; } else
+	lda	#0		;  return 0;
	rts			;} // drw4hand()

drewall	rts			;
animhnd	rts			;

pre_end
.align	$40
vararea
.end
