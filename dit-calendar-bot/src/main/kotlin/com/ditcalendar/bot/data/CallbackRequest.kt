package com.ditcalendar.bot.data

sealed class CallbackRequest

class UnassignCallbackRequest(val taskId: Long) : CallbackRequest()
class ReloadCallbackRequest(val calendarId: Long) : CallbackRequest()

const val unassignCallbackCommand = "unassign_"
const val reloadCallbackCommand = "reloadCalendar_"

fun parse(callbaBackData: String): CallbackRequest? =
        if (callbaBackData.startsWith(unassignCallbackCommand)) {
            val taskId: Long? = callbaBackData.substringAfter("_").toLongOrNull()
            if (taskId != null)
                UnassignCallbackRequest(taskId)
            else
                null
        } else if (callbaBackData.startsWith(reloadCallbackCommand)) {
            val calendarId: Long? = callbaBackData.substringAfter("_").toLongOrNull()
            if (calendarId != null)
                ReloadCallbackRequest(calendarId)
            else
                null
        } else
            null
