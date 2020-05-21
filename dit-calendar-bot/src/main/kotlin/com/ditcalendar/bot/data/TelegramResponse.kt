package com.ditcalendar.bot.data

sealed class TelegramResponse(val message: String)

class OnlyText(message: String) : TelegramResponse(message)
class WithInline(message: String, val callBackText: String, val callBackData: String,
                 val callbackNotificationText: String?) : TelegramResponse(message)