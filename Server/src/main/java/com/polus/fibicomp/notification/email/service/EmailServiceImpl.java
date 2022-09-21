package com.polus.fibicomp.notification.email.service;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.notification.email.dao.EmailMaintenanceDao;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationLog;
import com.polus.fibicomp.notification.pojo.NotificationLogRecipient;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.notification.pojo.NotificationType;
import com.polus.fibicomp.notification.render.EmailRenderFactory;
import com.polus.fibicomp.notification.render.EmailRenderService;
import com.polus.fibicomp.proposal.dao.ProposalDao;

@Transactional
@Service(value = "emailService")
public class EmailServiceImpl implements EmailService {

	protected static Logger logger = LogManager.getLogger(EmailServiceImpl.class.getName());

	@Autowired
	@Qualifier(value = "mailSender")
	public JavaMailSenderImpl mailSender;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	public ProposalDao proposalDao;

	@Autowired
	private EmailMaintenanceDao emailMaintenanceDao;

	@Autowired
	private EmailRenderFactory emailRenderFactory;

	private ExecutorService executorService = Executors.newCachedThreadPool();

	private static final String TO = "TO";
	private static final String CC = "CC";
	private static final String BCC = "BCC";

	@Value("${notification.attachment.filepath}")
	private String notificationAttachmentFilePath;

	@Override
	public EmailServiceVO sendEmail(EmailServiceVO emailServiceVO) {
		if (emailServiceVO.getNotificationTypeId() == null) {
			emailServiceVO.setBody(replacePlaceHolder(emailServiceVO.getBody(), emailServiceVO.getPlaceHolder()));
			emailServiceVO.setSubject(replacePlaceHolder(emailServiceVO.getSubject(), emailServiceVO.getPlaceHolder()));
			threadForMail(emailServiceVO);
			return emailServiceVO;
		}
		NotificationType notificationType = emailMaintenanceDao.fetchNotificationById(emailServiceVO.getNotificationTypeId());
		if (notificationType == null) {
			logger.info("notification is not found for notification id : {}" , emailServiceVO.getNotificationTypeId());
			return emailServiceVO;
		}
		emailServiceVO = getEmailDetails(emailServiceVO, notificationType);
		if (notificationType.getIsActive().equals("N")) {
			return emailServiceVO;
		}
		if (notificationType.getPromptUser().equals("Y") && emailServiceVO.getPrompted() == null) {
			emailServiceVO.setPrompted(false);
			return emailServiceVO;
		}
		Set<NotificationRecipient> recipients = new HashSet<>();
		if (emailServiceVO.getRecipients() != null && !emailServiceVO.getRecipients().isEmpty()) {
			recipients.addAll(emailServiceVO.getRecipients());
		}
		recipients.addAll(notificationType.getNotificationRecipient());
		emailServiceVO.setRecipients(recipients);
		threadForMail(emailServiceVO);
		return emailServiceVO;
	}

	@Override
	public void logEmail(EmailServiceVO emailServiceVO) {
		NotificationLog notificationLog = new NotificationLog();
		String sendFlag = null;
		String mailStatus = null;
		Integer recipientCount = emailServiceVO.getRecipients().size();
		notificationLog.setModuleItemKey(emailServiceVO.getModuleItemKey());
		notificationLog.setModuleCode(emailServiceVO.getModuleCode());
		notificationLog.setNotificationTypeId(emailServiceVO.getNotificationTypeId());
		notificationLog.setFromUserEmailId(mailSender.getUsername());
		notificationLog.setSendDate(commonDao.getCurrentTimestamp());
		notificationLog.setMessage(emailServiceVO.getBody());
		notificationLog.setSubject(emailServiceVO.getSubject());
		notificationLog.setErrorMessage(emailServiceVO.getErrorMessage());
		mailStatus = emailServiceVO.getErrorMessage() == null ? "Y" : "N";
		notificationLog.setIsSuccess(mailStatus);
		emailMaintenanceDao.createNotificationLog(notificationLog);
		for (NotificationRecipient recipient : emailServiceVO.getRecipients()) {
			NotificationLogRecipient notificationLogRecipient = new NotificationLogRecipient();
			sendFlag = recipient.getSendFlag();
			if (mailStatus.equals("N")) {
				sendFlag = "E";
			}
			notificationLogRecipient.setNotificationLogId(notificationLog.getNotificationLogId());
			notificationLogRecipient.setMailSentFlag(sendFlag);
			notificationLogRecipient.setToUserEmailId(recipient.getEmailAddress());
			if ((recipientCount > -1) && (recipientCount--) % 2000 == 0) {
				threadSleep();
			}
			emailMaintenanceDao.saveOrUpdate(notificationLogRecipient);
		}
	}

	private boolean isEmailTestEnabled() {
		return commonDao.getParameterValueAsBoolean("EMAIL_NOTIFICATION_TEST_ENABLED");
	}

	private String getTestMessageBody(String body, Set<NotificationLogRecipient> toAddresses, Set<NotificationLogRecipient> ccAddresses, Set<NotificationLogRecipient> bccAddresses) {
		StringBuilder testEmailBody = new StringBuilder("");
		testEmailBody.append("-----------------------------------------------------------<br/>");
		testEmailBody.append("TEST MODE<br/>");
		testEmailBody.append("In Production mode this mail will be sent to the following... <br/>");
		if (CollectionUtils.isNotEmpty(toAddresses)) {
			testEmailBody.append("TO: ");
			testEmailBody.append(appendInMessageBody(toAddresses));
		}
		if (CollectionUtils.isNotEmpty(ccAddresses)) {
			testEmailBody.append("CC: ");
			testEmailBody.append(appendInMessageBody(ccAddresses));
		}
		if (CollectionUtils.isNotEmpty(bccAddresses)) {
			testEmailBody.append("BCC: ");
			testEmailBody.append(appendInMessageBody(bccAddresses));
		}
		testEmailBody.append("<br/>-----------------------------------------------------------");
		return testEmailBody.toString() + "<br/>" + body;
	}

	private Set<String> appendInMessageBody(Set<NotificationLogRecipient> notificationLogRecipients) {
		Set<String> emails = new HashSet<>();
		for (NotificationLogRecipient notificationLogRecipient : notificationLogRecipients) {
			if (notificationLogRecipient.getMailSentFlag() != null && !notificationLogRecipient.getMailSentFlag().equals("N")) {
				emails.add(notificationLogRecipient.getToUserEmailId());
			}
		}
		return emails;
	}

	private String getEmailNotificationTestAddress() {
		return commonDao.getParameterValueAsString(Constants.EMAIL_NOTIFICATION_TEST_ADDRESS);
	}

	@Override
	public synchronized void send(EmailServiceVO emailServiceVO) {
		logger.info("Received request for mail sending");
		String subject = emailServiceVO.getSubject();
		String body = emailServiceVO.getBody();
		File file = emailServiceVO.getFileName();
		if (mailSender != null) {
			if (CollectionUtils.isEmpty(emailServiceVO.getRecipients())) {
				return;
			}
			Map<String, Set<NotificationLogRecipient>> emailAddress = groupEmailRecipients(emailServiceVO);
			if (emailAddress == null) {
				return;
			}
			MimeMessage message = mailSender.createMimeMessage();
			MimeMessageHelper helper = null;
			try {
				helper = new MimeMessageHelper(message, true, Constants.CHARSET);
				if (StringUtils.isNotBlank(subject)) {
					helper.setSubject(subject);
				} else {
					logger.warn("Sending message with empty subject.");
				}
				addEmailAttachment(file, helper);
				if (isEmailTestEnabled()) {
					helper.setText(
							getTestMessageBody(body, emailAddress.get(TO), emailAddress.get(CC), emailAddress.get(BCC)), true);
					helper = setTestEmail(helper);
				} else {
					helper.setText(body, true);
					setEmailRecipients(emailAddress.get(TO), emailAddress.get(CC), emailAddress.get(BCC), helper);
				}
				helper.setFrom(mailSender.getUsername(), mailSender.getJavaMailProperties().getProperty("mail.smtp.personnel"));
				executorService.execute(() -> mailSender.send(message));
				threadSleep();
			} catch (MessagingException ex) {
				logger.error("Failed to create mime message helper.", ex);
				emailServiceVO.setErrorMessage("Failed to create mime message helper." + ex);
			} catch (UnsupportedEncodingException ex) {
				logger.error("Error occured.", ex);
			} finally {
				if (file != null) {
					deleteAttachment(file);
				}
			}
		} else {
			logger.info("Failed to send email due to inability to obtain valid email mailSender, please check your configuration.");
			emailServiceVO.setErrorMessage("Failed to send email due to inability to obtain valid email mailSender, please check your configuration.");
		}
		logEmail(emailServiceVO);
	}

	@Override
	public void deleteAttachment(File file) {
		File directory = new File(notificationAttachmentFilePath + File.separator + "NotificationAttatchments");
		if (directory.isDirectory()) {
			File[] fileData = directory.listFiles();
			if (fileData != null) {
				for (File attachment : fileData) {
					if (file.getName().equals(attachment.getName())) {
						try {
							Files.deleteIfExists(Paths.get(file.getAbsolutePath()));
						} catch (Exception e) {
							logger.error("error in deleteAttachment {}", e.getMessage());
						}
					}
				}
			}
		}
	}

	private void addEmailAttachment(File file, MimeMessageHelper helper) {
		try {
			if (file != null) {
				FileSystemResource attachment = new FileSystemResource(file);
				helper.addAttachment(file.getName(), attachment);
			}
		} catch (MessagingException e) {
		 logger.error("error in addEmailAttachment {}", e.getMessage());
		}
	}

	public void setEmailRecipients(Set<NotificationLogRecipient> toAddresses, Set<NotificationLogRecipient> ccAddresses, Set<NotificationLogRecipient> bccAddresses, MimeMessageHelper helper) {
		if (CollectionUtils.isNotEmpty(toAddresses)) {
			for (NotificationLogRecipient toAddress : toAddresses) {
				try {
					if (toAddress.getMailSentFlag() != null && !toAddress.getMailSentFlag().equals("N")) {
						helper.addTo(toAddress.getToUserEmailId());
					}
				} catch (Exception ex) {
					logger.error("Could not set to address : {}", ex.getMessage());
				}
			}
		}
		if (CollectionUtils.isNotEmpty(ccAddresses)) {
			for (NotificationLogRecipient ccAddress : ccAddresses) {
				try {
					if (ccAddress.getMailSentFlag() != null && !ccAddress.getMailSentFlag().equals("N")) {
						helper.addCc(ccAddress.getToUserEmailId());
					}
				} catch (Exception ex) {
					logger.error("Could not set to address:", ex);
				}
			}
		}
		if (CollectionUtils.isNotEmpty(bccAddresses)) {
			for (NotificationLogRecipient bccAddress : bccAddresses) {
				try {
					if (bccAddress.getMailSentFlag() != null && !bccAddress.getMailSentFlag().equals("N")) {
						helper.addBcc(bccAddress.getToUserEmailId());
					}
				} catch (Exception ex) {
					logger.error("Could not set to address:", ex);
				}
			}
		}
	}

	public Map<String, Set<NotificationLogRecipient>> groupEmailRecipients(EmailServiceVO emailServiceVO) {
		Map<String, Set<NotificationLogRecipient>> emailAddress = new HashMap<>();
		Set<NotificationLogRecipient> toAddresses = new HashSet<>();
		Set<NotificationLogRecipient> ccAddresses = new HashSet<>();
		Set<NotificationLogRecipient> bccAddresses = new HashSet<>();
		boolean isMailIdsNull = true;
		emailServiceVO.setRecipients(getEmailAddresses(emailServiceVO));
		for (NotificationRecipient recipient : emailServiceVO.getRecipients()) {
			NotificationLogRecipient notificationLogRecipient = new NotificationLogRecipient();
			if (recipient.getEmailAddress() != null) {
				isMailIdsNull = false;
				notificationLogRecipient.setMailSentFlag(recipient.getSendFlag());
				notificationLogRecipient.setToUserEmailId(recipient.getEmailAddress());
				if (recipient.getRecipientType().equals(TO)) {
					toAddresses.add(notificationLogRecipient);
				} else if (recipient.getRecipientType().equals(CC)) {
					ccAddresses.add(notificationLogRecipient);
				} else {
					bccAddresses.add(notificationLogRecipient);
				}
			}
		}
		if (isMailIdsNull) {
			return null;
		}
		emailAddress.put(TO, toAddresses);
		emailAddress.put(CC, ccAddresses);
		emailAddress.put(BCC, bccAddresses);
		return emailAddress;
	}

	private Set<NotificationRecipient> getEmailAddresses(EmailServiceVO emailServiceVO) {
		Set<NotificationRecipient> recipients = new HashSet<>();
		Set<NotificationRecipient> recipientSet = new HashSet<>();
		Integer moduleCode = emailServiceVO.getModuleCode();
		String moduleItemKey = emailServiceVO.getModuleItemKey();
		Integer recipientCount = emailServiceVO.getRecipients().size();
		Integer subModuleCode = (emailServiceVO.getSubModuleCode() != null ? Integer.parseInt(emailServiceVO.getSubModuleCode()) : 0);
		String subModuleItemKey = emailServiceVO.getSubModuleItemKey();
		for (NotificationRecipient recipient : emailServiceVO.getRecipients()) {
			if (recipient.getRoleTypeCode() == null) {
				if (recipient.getEmailAddress() == null) {
					NotificationLogRecipient notificationLogRecipient = emailMaintenanceDao.getEmailAndFlag(recipient.getRecipientPersonId(), emailServiceVO.getNotificationTypeId());
					recipient.setEmailAddress(notificationLogRecipient.getToUserEmailId());
					recipient.setSendFlag(notificationLogRecipient.getMailSentFlag());
				} else {
					recipient.setSendFlag("Y");
				}
				if (recipients != null) {
					recipients.add(recipient);
				}
			} else {
				recipients = emailMaintenanceDao.getRoleEmail(recipient.getRoleTypeCode(), recipient.getRecipientType(),
						moduleCode, moduleItemKey,emailServiceVO.getNotificationTypeId(), subModuleCode, subModuleItemKey);
			}
			if (recipients != null && !recipients.isEmpty()) {
				recipientSet.addAll(recipients);
			}
			if ((recipientCount > -1) && (recipientCount--) % 2000 == 0) {
				threadSleep();
			}
		}
		return recipientSet;
	}

	public EmailServiceVO getEmailDetails(EmailServiceVO emailServiceVO, NotificationType notificationType) {
		EmailRenderService emailRenderService = null;
		Map<String, String> placeHolder = new HashMap<>();
		String moduleCode = emailServiceVO.getModuleCode().toString();
		String subModuleCode = emailServiceVO.getSubModuleCode() == null ? "0" : emailServiceVO.getSubModuleCode();
		try {
			emailRenderService = emailRenderFactory.getRenderService(moduleCode, subModuleCode);
			if (emailServiceVO.getSubModuleItemKey() != null && !emailServiceVO.getSubModuleItemKey().equals("0")) {
				placeHolder = emailRenderService.getPlaceHolderData(emailServiceVO.getSubModuleItemKey());
			} else {
				if (emailServiceVO.getModuleItemKey() != null) {
					placeHolder = emailRenderService.getPlaceHolderData(emailServiceVO.getModuleItemKey());
				}
			}
			if (emailServiceVO.getPlaceHolder() != null && !emailServiceVO.getPlaceHolder().isEmpty()) {
				placeHolder.putAll(emailServiceVO.getPlaceHolder());
			}
		} catch (Exception e) {
			logger.error("error in getEmailDetails : {}", e.getMessage());
			throw new ApplicationException("error in getEmailDetails", e, Constants.JAVA_ERROR);
		}
		if ((emailServiceVO.getNotificationTypeId() != null && emailServiceVO.getBody() == null) || (emailServiceVO.getNotificationTypeId() == null && emailServiceVO.getBody() != null)) {
			emailServiceVO.setBody(replacePlaceHolder(notificationType.getMessage(), placeHolder));
		}
		if ((emailServiceVO.getNotificationTypeId() != null && emailServiceVO.getSubject() == null) || (emailServiceVO.getNotificationTypeId() == null && emailServiceVO.getSubject() != null)) {
			emailServiceVO.setSubject(replacePlaceHolder(notificationType.getSubject(), placeHolder));
		}
		return emailServiceVO;
	}

	public MimeMessageHelper setTestEmail(MimeMessageHelper helper) {
//.....................This code is for attachments in future......................
//		FileSystemResource file = new FileSystemResource(new File("C:\\Users\\arjunkr\\Pictures\\Screenshots\\Screenshot (1).png"));
//		helper.addAttachment("attachment", file);
		String toAddress = getEmailNotificationTestAddress();
		if (StringUtils.isNotBlank(toAddress)) {
			try {
				helper.addTo(toAddress);
			} catch (MessagingException e) {
				logger.error("error in setTestEmail : {}", e.getMessage());
			}
		}
		return helper;
	}

	public String replacePlaceHolder(String message, Map<String, String> placeHolder) {
		if (message != null) {
			return render(message, placeHolder);
		}
		return "";
	}

	public String render(String text, Map<String, String> replacementParameters) {
		if (replacementParameters != null) {
			for (String key : replacementParameters.keySet()) {
				text = StringUtils.replace(text, key, replacementParameters.get(key));
			}
		}
		return text;
	}

	private void threadForMail(EmailServiceVO emailServiceVO) {
		if (!executorService.isTerminated()) {
			executorService.execute(() -> send(emailServiceVO));
		}
	}

	public void threadSleep() {
		try {
			Thread.sleep(10000);
		} catch (Exception e) {
			logger.error("Error occured in threadSleep : {}", e.getMessage());
		}
	}

}
