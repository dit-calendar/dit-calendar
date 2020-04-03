package com.ditcalendar.bot.parsing

import com.ditcalendar.bot.data.*
import com.ditcalendar.bot.data.core.Base
import com.ditcalendar.bot.error.DitBotError
import com.ditcalendar.bot.error.InvalidRequest
import com.ditcalendar.bot.error.UnassigmentError
import com.github.kittinunf.fuel.core.FuelError
import com.github.kittinunf.result.Result

fun parseResponse(result: Result<Base, Exception>): TelegramResponse =
        when (result) {
            is Result.Success ->
                when (val base = result.value) {
                    is DitCalendar -> OnlyText(base.toMarkdown() + System.lineSeparator())
                    is TaskForUnassignment ->
                        WithInline("*erfolgreich hinzugefügt zu:*" + System.lineSeparator() + base.toMarkdown(),
                                "test", "test_antwort")
                    is TaskForAssignment -> OnlyText("nicht implementiert")
                    is TaskAfterUnassignment -> OnlyText(base.toMarkdown())
                    else -> OnlyText("interner server Fehler")
                }
            is Result.Failure -> {
                result.error.printStackTrace()
                OnlyText(when (val error = result.error) {
                    is FuelError -> {
                        when (error.response.statusCode) {
                            403 -> "Bot fehlen notwendige Zugriffsrechte"
                            404 -> "Kalendar oder Task nicht gefunden"
                            503 -> "Server nicht erreichbar, versuchs nochmal"
                            else -> "unbekannter Fehler"
                        }
                    }
                    is DitBotError -> {
                        when (error) {
                            is InvalidRequest -> "fehlerhafte Anfrage"
                            is UnassigmentError -> "fehlgeschlagen"
                        }
                    }
                    else -> "unbekannter Fehler"
                })
            }
        }