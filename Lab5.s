;UNIVERSIDAD DEL VALLE DE GUATEMALA
;IE2023 Programación de Microcontroladores
;Autor:		Mónica Alfaro
;Compilador:	pic-as (v2.36), MPLABX (v6.00)
;
;Programa:	Laboratorio5 (Contador binario de 8 bits con 2 botones de 
;				incremento y decrementO en displays altenando
    ;				 con uso de tmr0		
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

var: DS 1
bandera: DS 1
    
display_variable: DS 2
    
nibble: DS 2
    
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
    
    BANKSEL PORTA
;Interrupción de PORTB
I_RBIF:
    btfsc INTCON, 0	    ; Revisa si la bandera IRBIF está activada
			    ;?antes estaba en Btfss pero aca estaba toda la info
			    ;de incremento y decremento de botones. 
    call botonesB
        
;Interrupción de TMR0

I_T0IF:
    btfsc   T0IF	    ;Ver si la bandera T0IF es 0, me salto una línea
    call    interrupcion_tmr0	    ;Pero si no es 0 (es 1) me voy a la subrutina 
    
    
    
    
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
;-------------Tabla display antes de código-------------------
contadorhexa: ;Entradas en A y salidas en B
   ;Tablas para cátodo común
   
   clrf PCLATH		    ;Program Counter
   bsf	PCLATH, 0 
   ;--------------Volviendo de 4 bits---------------------------
   andlw 0x0f		    ;Solo puedo llegar a 16 con el and.
   addwf PCL		    ;PC= PCLATH+ PCL +W
   
   retlw 00111111B ; 0	    ;No usamos return normal,ya que esa devuelve una lit
   retlw 00000110B  ;1
   retlw 01011011B  ;2
   retlw 01001111B  ;3
   retlw 01100110B  ;4
   retlw 01101101B  ;5
   retlw 01111101B  ;6
   retlw 00000111B  ;7
   retlw 01111111B  ;8
   retlw 01101111B  ;9
   retlw 01110111B  ;A
   retlw 01111100B  ;b
   retlw 00111001B  ;C
   retlw 01011110B  ;d
   retlw 01111001B  ;E
   retlw 01110001B  ;F
   
   /*A la hora de conectarlo se ve así
   Bit0 del puerto = Línea superior (pin 4 de arriba)
   Bit1 del puerto = Línea inferior derecha (pin 4 de abajo)
   Bit2 del puerto = Línea 
   Bit3 del puerto = Línea 
   Bit4 del puerto = Línea 
   Bit5 del puerto = Línea
   Bit5 del puerto = Línea 


   
   */
   
main:    
    call configuracion_inicial
    call interrupcion_puertoB
    call interrupciones
    call config_clck
    call config_tmr0

    ;Limpieza para iniciar en 00
    ;---> Puertos
    BANKSEL PORTC
    ;contaodor
    clrf PORTB 
    clrf PORTD
    ;display
    clrf PORTA
    ;transistores
    clrf PORTC
    
    ;---> Variables
    clrf var
    clrf bandera
    clrf display_variable
    clrf nibble
 
    
;*******************************************************************************
;LOOP INFINITO
;*******************************************************************************
    
loop: 
   ;Ingresando un valor (prueba)

   ;movlw 0x14
   ;movwf var
   
   call separacion_variable_nibbles
   call preparacion_displays
   
   
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
    
    clrf TRISA		    ;Todo el puerto A como salida (Displays)
    
    movlw 0b11111100	    ;2 bits de salida para puerto c(transistores mux)
    movwf TRISC
    ;Entradas
    
    movlw 0b00000011	    ;2 bits de entrada para puerto b(Botones Contador)
    movwf TRISB
    
    return

config_clck:
    
    banksel OSCCON	    ;Vamos al banco 1, donde está el OSCCON 
    bcf OSCCON, 4	    ;Los bits 5 y 6 del registro OSCCON son IRCF y 
    bsf OSCCON, 5	    ;la combinación de esos bits determina el FOSC
    bcf OSCCON, 6	    ;IRCF=010  250khZ --> verificar ircf en datasheet 
    bsf OSCCON, 0	    ;Selección de reloj interno con el bit 0 (SCS)
    return
 
config_tmr0:
    banksel OPTION_REG	    ;Vamos al banco 1
    bcf OPTION_REG, 5; T0CS		    ;? RELOJ INTERNO

    bcf OPTION_REG, 3 ;PSA		    ;PRESCALER
    
    bsf OPTION_REG, 2; PS2
    bcf OPTION_REG, 1 ;PS1 --> 64 
    bsf OPTION_REG, 0 ;PS0  ;PS=101 --> verificar en hoja de datos
    banksel PORTA
    call reiniciar_tmr0 ;optimización del reinicio
    
    return

reiniciar_tmr0:
    movlw 254                     ;251
    movwf TMR0
    bcf   T0IF
    return
;-------------------RECORDAR FORMULA PARA SABER LA FRECUENCIA DEL TIMER-----------
;   TEMPORIZACIÓN=TOSC*TMR0*PRESCALER
;   FOSC(frecuencia de oscilación)= 250 khz
;   TOSC (periodo de oscilación)= 1/250 000 = 0.000004 O 4 micro segundos
;   TMR0 = 256- n
;   N=VALOR A CARGAR EN TMR0
;   prescaler= 101 equivale a 64 (revisar datasheet)
;Entonces 
;	N = 254
        
    
interrupciones:
    
    banksel INTCON
    clrf INTCON
    
  
    bsf INTCON, 6	    ; Habilita las interrupciones Periféricas PEIE
   
    ;Interrupciones Puerto B
    bsf INTCON, 3	    ; Habilita las interrupciones RBIE
    bsf INTCON, 0	    ; Habilita las interrupciones RBIF
    bsf	T0IE
    bcf	T0IF
   
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
	
separacion_variable_nibbles:
  
    movf PORTD, w
    movwf var

    
    ;------------------------Ingresando el valor a displays---------------------
    
    ;call contadorhexa	    
    ;movwf PORTC		    ;Y lo muestro en el puerto C
    
    
    movf var,w
    andlw 0x0f
    
    ;Separación
    movwf  nibble
    swapf var, w
    andlw 0x0f
    movwf nibble+1
    return
    
preparacion_displays:
    movf nibble, w
    call contadorhexa
    movwf display_variable
    movf nibble+1, w
    call contadorhexa
    movwf display_variable+1
    return

interrupcion_tmr0:
    banksel PORTA
    call reiniciar_tmr0
    clrf PORTC
    BTFSC bandera, 0
    goto display_1
    ;goto display_0

display_0:
    movf    display_variable, w
    movwf   PORTA   ;DISPLAY
    bsf	    PORTC, 0 ;tRANSISOTOR
    goto    siguiente_display
display_1:  
    movf    display_variable+1, w
    movwf   PORTA   ;DISPLAY
    bsf	    PORTC, 1 ;tRANSISOTOR
    goto    siguiente_display
siguiente_display:
    movlw 1
    xorwf bandera, F	;cambiando de display
    
    ;xor = Solo si a o b son 1(1 opción es 1), salida es 1
    
    
    return
			    
END
 