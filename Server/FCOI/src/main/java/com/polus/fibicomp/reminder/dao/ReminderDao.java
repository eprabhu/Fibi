package com.polus.fibicomp.reminder.dao;

import java.sql.ResultSet;
import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.reminder.pojo.ReminderNotification;

@Service
public interface ReminderDao {

	/**
	 * This method is used to fetch all active remainders
	 * @return active remainders
	 */
	public List<ReminderNotification> fetchAllActiveReminders();

	/**
	 * This method is used to get the data to replace the notification content
	 * @param days
	 * @param procedureName
	 * @return data to replace the mail content
	 */
	public ResultSet getReminderData(Integer days, String procedureName);

}
