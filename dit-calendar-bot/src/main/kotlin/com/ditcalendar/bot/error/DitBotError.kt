package com.ditcalendar.bot.error

sealed class DitBotError(description: String) : RuntimeException(description)

class InvalidRequest : DitBotError("request invalid")
class UnassigmentError: DitBotError("error during unassigment")
