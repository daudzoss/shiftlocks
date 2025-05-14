;;; 
solvlen	.byte	(+)-tosolve
tosolve	.byte	$33,$20,$03,$0f	; 3 CO
	.byte	$0c,$20,$0f,$06	; L OF
	.byte	$20,$34,$20,$40	;  4 _
	.byte	$57		; @
+

gamewon	ldy	#4		;uint1_t gamewon(void) {
	ldx	#0		; uint2_t x;
-	lda	STACKHT+8-1,y	; for (uint8_t y = 4; y >= 1; y--) {
	cmp	#7		;
	bcc	+		;  if (STACKHT[(8-1)+y] >= 7) // enough for 4x i
	inx			;   x++;
+	dey			;
	bne	-		; }
	cpx	#3		;
	bcc	+		;
	ldy	#1		;
+	tya			; return (x >= 3);
	rts			;} // gamewon()
