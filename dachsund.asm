;;; Walker Dachsund, veteran physician not veterinarian despite the name
abillen	.byte	(+)-ability
ability	.byte	$14,$08,$12,$05	; THRE
	.byte	$01,$14,$40,$20	; AT_
	.byte	$09,$0e,$16,$05	; INVE
	.byte	$13,$14,$40,$20	; ST_
	.byte	$1f,$14,$08,$12	; <THR
	.byte	$05,$01,$14,$20	; EAT
	.byte	$04,$12,$01,$17	; DRAW
	.byte	$20,$0f,$0e,$05	;  ONE
+

double1	jmp	threatr		;

double2	jmp	investr		;

double3	jmp	threatl		;

double4	jmp	drawone		;
