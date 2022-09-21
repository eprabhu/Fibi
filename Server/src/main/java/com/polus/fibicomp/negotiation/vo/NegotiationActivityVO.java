package com.polus.fibicomp.negotiation.vo;

import java.util.List;

import com.polus.fibicomp.negotiation.pojo.NegotiationsActivity;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAttachment;

public class NegotiationActivityVO {

	String acType;
	private NegotiationsActivity negotiationActivity;
	private List<NegotiationsAttachment> attachment;
	private NegotiationsAttachment attachments;
	Integer negotiationsAttachmentId;
	
	public String getAcType() {
		return acType;
	}
	public void setAcType(String acType) {
		this.acType = acType;
	}

	public List<NegotiationsAttachment> getAttachment() {
		return attachment;
	}
	public void setAttachment(List<NegotiationsAttachment> attachment) {
		this.attachment = attachment;
	}
	public NegotiationsActivity getNegotiationActivity() {
		return negotiationActivity;
	}
	public void setNegotiationActivity(NegotiationsActivity negotiationActivity) {
		this.negotiationActivity = negotiationActivity;
	}
	public NegotiationsAttachment getAttachments() {
		return attachments;
	}
	public void setAttachments(NegotiationsAttachment attachments) {
		this.attachments = attachments;
	}
	public Integer getNegotiationsAttachmentId() {
		return negotiationsAttachmentId;
	}
	public void setNegotiationsAttachmentId(Integer negotiationsAttachmentId) {
		this.negotiationsAttachmentId = negotiationsAttachmentId;
	}

}
