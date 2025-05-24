;;;
solvlen	.byte	(+)-tosolve
tosolve	.byte	$01,$20,$03,$0f	; 1 CO
	.byte	$0c,$20,$0f,$06	; L OF
	.byte	$20,$30,$20,$5b	;  0 #
	.byte	$40		; _
+

gamewon	ldx	#1		;uint1_t gamewon(void) {
	ldy	#0		;
-	iny			; for (register uint8_t y = 1; y < 4; y++) {
	tya			;
	pha;1:current stack# 1~4;
	lda	STACKHT+8-1,y	;
	tay			;  int8_t y_ = STACKHT[(8-1)+y]; // stacks 8~11
	cpy	#7		;
	bcc	++		;  if (y_ >= 7) { // enough to have 4x investig.
-	lda	ODRAWER,x	;   for (register uint8_t x = 0; x < 15; x+=2) {
	and	#$04		;    if (ODRAWER[(y*16 + x) | 1] & 0x04) // odds
	bne	++		;     break; // threat, skip stack!
	inx			;
	inx			;
	dey			;
	beq	+		;
	dey			;    y_ -= 2; // skip intervening investig. card
	bne	-		;
+	pla;1->0		;    if (y_ <= 0) // confirmed a no-threat stack
	lda	#1		;     return 1; // won
	rts			;   }
+	pla;1->0		;
	tay			;
	txa			;  }
	and	#$f0		;
	clc			;
	adc	#$11		;
	tax			;
	and	#$40		;
	beq	--		; }
	lda	#0		; return 0;
	rts			;} // gamewon()
