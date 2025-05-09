setzptr	.macro	p,m,ym=0,x=1,y=1;inline void setzptr(uint1_t p, void const* m,
	pha			;           uint16_t ym, uint8_t x, uint8_t y) {
	lda	#<\m		; static void* zp[];
	sta	ZP+\p+\p	;
	lda	#>\m		;
	sta	1+ZP+\p+\p	; zp[p] = (void*) m + ym * y + x;
.if (\y)
	cpy	#0		;
	beq	+		;
-	clc			;
	lda	ZP+\p+\p	;
	adc	#<\ym		;
	sta	ZP+\p+\p	;
	lda	1+ZP+\p+\p	;
	adc	#>\ym		;
	sta	1+ZP+\p+\p	;
	dey			;
	bne	-		;
+				;
.endif
.if (\x)
	txa			;
	clc			;
	adc	ZP+\p+\p	;
	sta	ZP+\p+\p	;
	lda	1+ZP+\p+\p	;
	adc	#0		;
	sta	1+ZP+\p+\p	;
.endif
	pla			; // x and y both 0, a restored (sr accordingly)
	.endm			;} // setzptr()

txtclip	.macro	buf=0,p=0,ini=1	;inline void txtclip(char* buf, uint8_t x,
	.if (\ini)		;                    uint8_t y, uint8_t a) {
	setzptr	\p,SCREENM,SCREENW; setzptr(p,SCREENM,SCREENW,x,y);
	 .if (\buf)
	setzptr	\p+1,\buf,0,0,0	;  setzptr(p+1,buf,0,0,0);
	 .endif
	.endif			;
	.if (\buf)
	tay			; static void* zp[];
	beq	+		; register uint8_t a, y;
-	dey			; y = a;
	lda	(ZP+\p+\p),y	; while (y--) 
	sta	(ZP+\p+\p+2),y	;  *(zp[p+1] + y) = a = *(zp[p] + y);
	cpy	#0		; y = 0;
	bne	-		; // a indeterminate, x and y both 0, z set
+
	.endif			;
	.endm			;} // txtclip()

replace	.macro	newstr,p=0,ini=1;inline void replace(char* newstr, uint8_t a) {
	.if (\ini)
	setzptr	\p+1,\newstr,0,0,0; setzptr(p+1,newstr,0,0,0);
	.endif
	tay			; static void* zp[];
	beq	+		; register uint8_t a, y;
-	dey			; y = a;
	lda	(ZP+\p+\p+2),y	; while (y--)
	sta	(ZP+\p+\p),y	;  *(zp[p] + y) = a = *(zp[p+1] + y);
	cpy	#0		; y = 0
	bne	-		; // a indeterminate, x and y both 0, z set
+
	.endm			;} // replace()

handmsg	.macro s1s2,s1l,s2l,bck=0;inline void handmsg(uint8_t* s1s2,uint8_t s1l,
	ldx	inhandx		;                     uint8_t s2l,uint8_t* bck){
	dex			; x = inhandx[0] - 1;
	ldy	inhandy		;
	dey			; y = inhandy[0] - 2;
	dey			;// 17 characters centered within row above hand
	lda	#\s1l		; a = s1l; // length of string 1
	txtclip	\bck		; txtclip(bck /*backing store*/, x, y, a);
	lda	#\s1l		; a = s1l; // length of string 1
	replace	\s1s2		; replace(s1s2); // show s1
	ldy	inhandy		;
	ldx	#6		;
-	iny			;
	dex			;// 15 characters centered within row below hand
	bne	-		; y = inhandy[0] + 6;
	ldx	inhandx		; x = inhandx[0];
	lda	#\s2l		; a = s2l; // length of string 2 imm. following
	txtclip	\bck+\s1l	; txtclip(x, y, a, SCRATCH + CLP1SZ);
	lda	#\s2l		; a = s2l; // length of string 2 imm. following
	replace	\s1s2+\s1l	; replace(rejmsg0);
	.endm			;} // handmsg()

printxy	.macro	bytes		;inline void printxy(register uint8_t& a, // len
	pha			;  register uint8_t& x,register uint8_t& y,//c,r
	txtclip			;  const uint8_t const* bytes) { // screencodes
	pla			; txtclip(0, 0, 1);
	replace	\bytes		; replace(bytes, 0, 1);
	.endm			;} // printxy()

digitxy	.macro	d,x,y,mask0f=$8f;inline void digitxy(const uint8_t* d, const
	lda	#$80		; uint8_t x, const uint8_t y, const uint8_t m) {
	and SCREENM+\x+SCREENW*\y
	ora	\d		; register uint8_t a =SCREENM[x+SCREENW*y]&0x80;
	.if \mask0f
	 and	#\mask0f	; if (m) a &= m;
	.endif
	ora	#$30		; SCREENM[x+SCREENW*y] = a | *d | '0';
	sta SCREENM+\x+SCREENW*\y
	.endm			;} // digitxy()
