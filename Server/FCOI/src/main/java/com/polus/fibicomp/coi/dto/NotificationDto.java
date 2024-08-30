package com.polus.fibicomp.coi.dto;

import java.util.List;

import org.bouncycastle.cms.Recipient;

import com.polus.core.notification.pojo.NotificationRecipient;

import lombok.Data;

@Data
public class NotificationDto {
	//private String moduleItemKey;
	private Integer disclosureId;
	private String notificationTypeId;
	private Integer projectTypeCode;
	private Integer projectId;
	private List<NotificationRecipient> recipients;
	private String description;
	private String message;
	private String subject;
	
	
}
