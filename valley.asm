;;; 
solvlen	.byte	(+)-tosolve
tosolve	.byte	$33,$20,$03,$0f	; 3 CO
	.byte	$0c,$20,$0f,$06	; L OF
	.byte	$20,$34,$20,$40	;  4 _
	.byte	$57		; @
+
	
gamewon	ldx	#0		;uint1_t gamewon(void) {
	ldy	#0		;
	sty	TEMPVAR		; TEMPVAR = 0;// 3..0:#i.cards; 7..4:#col w/>=4
-	iny			; for (register uint8_t y = 1; y < 4; y++) {
	tya			;
	pha;1:current stack# 1~4;
	lda	TEMPVAR		;
	and	#$30		;
	sta	TEMPVAR		;  TEMPVAR &= 0x30; // clear #i. cards, not #col
	lda	STACKHT+8-1,y	;
	tay			;  int8_t y_ = STACKHT[(8-1)+y]; // stacks 8~11
	cpy	#7		;
	bcc	+++		;  if (y_ >= 7) { // enough to have 4x investig.
-	lda	ODRAWER,x	;   for (register uint8_t x = 0; x < 15; x+=2) {
	cmp	#$04		;
	bcs	++		;    if (ODRAWER[(y*16 + x) | 0] < 4 ) {// evens
	inc	TEMPVAR		;     TEMPVAR++;
	lda	TEMPVAR		;
	bit	found4i		;
	beq	++		;     if (TEMPVAR & 0x04) {// found 4 inv. cards
	bit	in2stax		;
	beq	+		;      if (TEMPVAR & 0x20)  // for the 3rd time!
	pla;1->0		;
	lda	#1		;
	rts			;       return 1; // won
+	clc			;
	adc	#$10		;
	sta	TEMPVAR		;      TEMPVAR += 0x10;
	bne	++		;      break; // check the next stack
+	inx			;     }
	inx			;    }
	dey			;
	beq	++		;
	dey			;    y_ -= 2; // skip intervening investig. card
	bne	-		;   }
+	pla;1->0		;
	tay			;
	txa			;  }
	and	#$f0		;
	clc			;
	adc	#$10		;
	tax			;
	and	#$c0		;
	beq	--		; }
	lda	#0		; return 0;
	rts			;} // gamewon()
found4i	.byte	$04		;
in2stax	.byte	$20		;
