package com.ditcalendar.bot.service

import com.ditcalendar.bot.data.DitCalendar
import com.ditcalendar.bot.data.TelegramLink
import com.ditcalendar.bot.data.TelegramTaskAfterUnassignment
import com.ditcalendar.bot.data.TelegramTaskForUnassignment
import com.ditcalendar.bot.data.core.Base
import com.ditcalendar.bot.endpoint.AuthEndpoint
import com.ditcalendar.bot.endpoint.CalendarEndpoint
import com.ditcalendar.bot.endpoint.TelegramAssignmentEndpoint
import com.ditcalendar.bot.error.InvalidRequest
import com.ditcalendar.bot.error.UnassigmentError
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.flatMap
import com.github.kittinunf.result.map

const val unassignCallbackCommand = "unassign_"
const val reloadCallbackCommand = "reloadCalendar_"
const val assingWithNameCallbackCommand = "assignme_"
const val assingAnnonCallbackCommand = "assignmeAnnon_"

class DitCalendarService {

    private val calendarEndpoint = CalendarEndpoint()
    private val taskEndpoint = TelegramAssignmentEndpoint()
    private val authEndpoint = AuthEndpoint()

    fun executeCallback(telegramLink: TelegramLink, callbaBackData: String): Result<Base, Exception> =
            if (callbaBackData.startsWith(unassignCallbackCommand)) {
                val taskId: Long? = callbaBackData.substringAfter("_").toLongOrNull()
                if (taskId != null)
                    unassignUserFromTask(taskId, telegramLink)
                else
                    Result.error(InvalidRequest())
            } else if (callbaBackData.startsWith(reloadCallbackCommand)) {
                val calendarId: Long? = callbaBackData.substringAfter("_").toLongOrNull()
                if (calendarId != null)
                    getCalendarAndTask(calendarId)
                else
                    Result.error(InvalidRequest())
            } else if (callbaBackData.startsWith(assingWithNameCallbackCommand)) {
                executeTaskAssignmentCommand(telegramLink, callbaBackData)
            } else if (callbaBackData.startsWith(assingAnnonCallbackCommand)) {
                executeTaskAssignmentCommand(telegramLink.copy(firstName = null, userName = null), callbaBackData)
            } else
                Result.error(InvalidRequest())

    fun executeTaskAssignmentCommand(telegramLink: TelegramLink, opts: String): Result<Base, Exception> {
        val taskId: Long? = opts.substringAfter("_").toLongOrNull()
        return if (taskId != null)
            assignUserToTask(taskId, telegramLink)
        else
            Result.error(InvalidRequest())
    }

    fun executePublishCalendarCommand(opts: String?): Result<Base, Exception> {
        val calendarId: Long? = opts?.toLongOrNull()
        return if (calendarId != null)
            getCalendarAndTask(calendarId)
        else
            Result.error(InvalidRequest())
    }


    private fun getCalendarAndTask(calendarId: Long): Result<DitCalendar, Exception> {
        val tokenResul = authEndpoint.getToken()
        val calendarResult = tokenResul.flatMap { calendarEndpoint.readCalendar(calendarId, it) }
        val tasksResulst = tokenResul.flatMap { taskEndpoint.readTasks(calendarId, it) }

        return calendarResult.flatMap { calendar ->
            tasksResulst.map {
                calendar.apply { telegramTaskAssignments = it }
            }
        }
    }

    private fun assignUserToTask(taskId: Long, telegramLink: TelegramLink): Result<TelegramTaskForUnassignment, Exception> =
            authEndpoint.getToken()
                    .flatMap { taskEndpoint.assignUserToTask(taskId, telegramLink, it) }

    private fun unassignUserFromTask(taskId: Long, telegramLink: TelegramLink): Result<TelegramTaskAfterUnassignment, Exception> {
        return authEndpoint.getToken()
                .flatMap { taskEndpoint.unassignUserFromTask(taskId, telegramLink, it) }
                .fold({ Result.Success(it) }, { Result.error(UnassigmentError()) })
    }
}