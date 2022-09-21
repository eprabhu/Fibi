package com.polus.fibicomp.grantcall.vo;

import java.util.Set;

public class EmailNotificationVO {

	private String emailSubject;
	private String emailBody;
	private Set<String> emailRecipient;
	private Set<String> recipientRoleId;
	
	
	public String getEmailSubject() {
		return emailSubject;
	}
	public void setEmailSubject(String emailSubject) {
		this.emailSubject = emailSubject;
	}
	public String getEmailBody() {
		return emailBody;
	}
	public void setEmailBody(String emailBody) {
		this.emailBody = emailBody;
	}
	public Set<String> getEmailRecipient() {
		return emailRecipient;
	}
	public void setEmailRecipient(Set<String> emailRecipient) {
		this.emailRecipient = emailRecipient;
	}
	public Set<String> getRecipientRoleId() {
		return recipientRoleId;
	}
	public void setRecipientRoleId(Set<String> recipientRoleId) {
		this.recipientRoleId = recipientRoleId;
	}
	
}
