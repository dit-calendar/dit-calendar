package com.ditcalendar.bot.formatter

import com.ditcalendar.bot.data.*
import com.ditcalendar.bot.data.core.Base
import com.ditcalendar.bot.error.DitBotError
import com.ditcalendar.bot.error.InvalidRequest
import com.ditcalendar.bot.error.UnassigmentError
import com.github.kittinunf.fuel.core.FuelError
import com.github.kittinunf.result.Result
import javax.enterprise.context.ApplicationScoped


@ApplicationScoped
class StringFormatter(val markdownFormatter: MarkdownFormatter) {

    fun parseResponse(result: Result<Base, Exception>): TelegramResponse =
            when (result) {
                is Result.Success -> parseSuccess(result.value)
                is Result.Failure -> {
                    result.error.printStackTrace()
                    parseError(result.error)
                }
            }

    private fun parseSuccess(result: Base): TelegramResponse = markdownFormatter.run {
        when (result) {
            is DitCalendar ->
                WithInline(result.toMarkdown() + System.lineSeparator(), "reload", "$reloadCallbackCommand${result.entryId}")
            is TaskForUnassignment ->
                WithInline("*erfolgreich hinzugefügt zu:*" + System.lineSeparator() + result.toMarkdown(),
                        "unassign me", "unassign_${result.taskId}")
            is TaskForAssignment ->
                OnlyText("nicht implementiert")
            is TaskAfterUnassignment ->
                OnlyText(result.toMarkdown())
            else ->
                OnlyText("interner server Fehler")
        }
    }

    private fun parseError(error: Exception): TelegramResponse =
            OnlyText(when (error) {
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