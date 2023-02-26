;UNIVERSIDAD DEL VALLE DE GUATEMALA
;IE2023 Programación de Microcontroladores
;Autor:		Mónica Alfaro
;Compilador:	pic-as (v2.36), MPLABX (v6.00)
;
;Programa:	PRELaboratorio5 (Contador bonario de 8 bits con 2 botones de 
;				 incremento y decrementO)		
;
;				
;Dispositivo:	PIC16F887
;Hardware:	LEDs en el puerto D, botones en el puerto B
;
;Creado:	       19 de febrero , 2023
;Última modificación:  19 de febrero , 2023


PROCESSOR 16F887
#include <xc.inc>
;configuration word 1
 CONFIG FOSC=INTRC_NOCLKOUT //OSCILADOR INTERNO SIN SALIDAS
 CONFIG WDTE=OFF //WDT DISEABLED (REINICIO REPETITIVO DEL PIC)
 CONFIG PWRTE=OFF //PWRT ENABLED (ESPERA DE 72ms AL INICIAR)
 CONFIG MCLRE=OFF //EL PIN DE MCLR SE UTILIZA COMO I/0
 CONFIG CP=OFF	//SIN PROTECCIÓN DE CÓDIGO
 CONFIG CPD=OFF	//SIN PROTECCIÓN DE DATOS
 
 CONFIG BOREN=OFF //SIN REINICIO CUÁNDO EL VOLTAJE DE ALIMENTACIÓN BAJA DE 4V
 CONFIG IESO=OFF //REINCIO SIN CAMBIO DE RELOJ DE INTERNO A EXTERNO
 CONFIG FCMEN=OFF //CAMBIO DE RELOJ EXTERNO A INTERNO EN CASO DE FALLO
 CONFIG LVP=OFF //PROGRAMACIÓN EN BAJO VOLTAJE PERMITIDA
 
;configuration word 2
 CONFIG WRT=OFF	//PROTECCIÓN DE AUTOESCRITURA POR EL PROGRAMA DESACTIVADA
 CONFIG BOR4V=BOR40V //REINICIO ABAJO DE 4V, (BOR21V=2.1V)

PSECT udata_bank0	; common memory

    
 PSECT udata
 W_TEMP:
    DS 1
 STATUS_TEMP:
    DS 1

    
PSECT resVect, class=code, abs, delta=2
;----------------------------------VECTOR RESET---------------------------------
ORG 00h			    ;Posicion 0000h para el vector
resetVec:
    PAGESEL main
    goto main

PSECT code, delta=2, abs
ORG 100h		    ;Posicion para el codigo
 
;--------------------------VECTOR DE INTERRUPCIONES-----------------------------

PSECT code, delta=2, abs
 ORG 0x0004
PUSH:			    ;Parte de el código que menciona el datasheet.
    MOVWF W_TEMP
    SWAPF STATUS, W
    MOVWF STATUS_TEMP

;Interrupción de PORTB
I_RBIF:
    btfsc INTCON, 0	    ; Revisa si la bandera IRBIF está activada
			    ;?antes estaba en Btfss pero aca estaba toda la info
			    ;de incremento y decremento de botones. 
    call botonesB
        
    
POP:
    SWAPF STATUS_TEMP, W
    MOVWF STATUS
    SWAPF W_TEMP, F
    SWAPF W_TEMP, W
    RETFIE		    ;REGRESA DE LA INTERRUPCIÓN
;*******************************************************************************
;CÓDIGO PRINCIPAL
;*******************************************************************************
PSECT CODE, delta = 2, abs
 ORG 0x100

   
main:    
    call configuracion_inicial
    call interrupcion_puertoB
    call interrupciones
   

    BANKSEL PORTC
   
    clrf PORTB 
    clrf PORTD
 
    
;*******************************************************************************
;LOOP INFINITO
;*******************************************************************************
    
loop: 
   
   goto loop
    
;*******************************************************************************
;				CONFIGURACIONES
;*******************************************************************************
  
configuracion_inicial:
    
    banksel ANSEL	    ; Configuración de pines digitales
    clrf ANSEL		  
    clrf ANSELH		   
    
    banksel TRISB
    clrf TRISB
    
    banksel TRISB
    
    ;Salidas
    clrf TRISD		    ;Todo el puerto C como salida  (Contador)

    ;Entradas
    
    movlw 0b00000011	    ;2 bits de entrada para puerto b(Botones Contador)
    movwf TRISB
    
    return

 
    
interrupciones:
    
    banksel INTCON
    clrf INTCON
    
  
    bsf INTCON, 6	    ; Habilita las interrupciones Periféricas PEIE
   
    ;Interrupciones Puerto B
    bsf INTCON, 3	    ; Habilita las interrupciones RBIE
    bsf INTCON, 0	    ; Habilita las interrupciones RBIF
    
   
    bsf INTCON, 7	    ; Habilita las interrupciones Globales GIE
    
    return

;*******************************************************************************
;				SUBRUTINAS 
;*******************************************************************************
interrupcion_puertoB:
    
    BANKSEL IOCB
    BSF IOCB, 0
    BSF IOCB, 1		    ; HABILITANDO RB0 Y RB1 PARA LAS ISR DE RBIE
    
    BANKSEL OPTION_REG
    BCF OPTION_REG, 7	    ; HABILITANDO PULLUPS PUERTO B
    
    BSF WPUB, 0
    BSF WPUB, 1		    ; HABILITANDO LOS PULLUPS EN RB0 Y RB1
    
    RETURN
    
botonesB:
    revisarboton1:
	btfss PORTB, 0	    ; Revisa si el botón está presinoado (llegan 5v)
	incf PORTD, F	    ; Incrementa el contador en PORTC
	;Antirebote
	btfss PORTB, 0
	goto  $-1

	bcf INTCON, 0	    ; Limpiar bandera
    
    revisarboton2:		    
	btfss PORTB, 1	    ; Revisa si el botón está presinoado (llegan 5v)
	decf PORTD, F	    ; Decrementa el contador en PORTC
	;Antirebote
	btfss PORTB, 1
	goto  $-1
	bcf  INTCON, 0	    ; Limpiar bandera
	;
	return
    

			    
			    
END
 
