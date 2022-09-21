package com.polus.fibicomp.notification.email.service;

import java.io.File;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.service.NotificationService;

@Transactional
@Service(value = "emailService")
public interface EmailService extends NotificationService {

	/**
	 * This method is used to set the mail details before send the mail
	 * @param emailServiceVO
	 * @return object of EmailServiceVO
	 */
	public EmailServiceVO sendEmail(EmailServiceVO emailServiceVO);

	/**
	 * This method is used to log the mail success details
	 * @param emailServiceVO
	 */
	public void logEmail(EmailServiceVO emailServiceVO);
	
	/**
	 * This method will send the email
	 * @param emailServiceVO
	 */
	public void send(EmailServiceVO emailServiceVO);

	/**
	 * This method is used to delete attachment.
	 * @param file - File
	 */
	public void deleteAttachment(File file);

}
