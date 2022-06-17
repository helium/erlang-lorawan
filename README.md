# Erlang LoraWan

erlang-lorawan provides erlang modules to read and write LoRaWAN
1.0 frames from and to a hex or base64 encoded array of bytes.

The following structures are implemented (+ fields):

```
PHYPayload    (MHDR | MACPayload | MIC)
MACPayload    (FHDR | FPort | FRMPayload)
FHDR          (DevAddr | FCtrl | FCnt | FOpts)
```

The Following message types (MType) are implemented:

* JoinRequest
* RejoinRequest
* JoinAccept
* UnconfirmedDataUp
* UnconfirmedDataDown
* ConfirmedDataUp
* ConfirmedDataDown
* Proprietary

