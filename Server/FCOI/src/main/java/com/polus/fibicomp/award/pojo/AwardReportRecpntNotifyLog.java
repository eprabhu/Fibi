package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "AWARD_REPORT_RECPNT_NOTIFY_LOG")
public class AwardReportRecpntNotifyLog implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_REP_RECPNT_NOTIFY_LOG_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_NOTFY_RCPT_LOG_GENERATOR")
	@SequenceGenerator(name="AWARD_NOTFY_RCPT_LOG_GENERATOR", sequenceName = "AWARD_NOTFY_RCPT_LOG_GENERATOR", allocationSize=1)
	private Integer awardRepRecpntNotifyLogId;

	@Column(name = "AWARD_REPORT_TERM_RECIPIENT_ID")
	private Integer awardReportTermRecipientId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_REP_RECPNT_NOTFY_LOG_FK1"), name = "AWARD_REPORT_TERM_RECIPIENT_ID", referencedColumnName = "AWARD_REPORT_TERM_RECIPIENT_ID", insertable = false, updatable = false)
	private AwardReportTermRecipient reportTermRecipient;

	@Column(name = "AWARD_REPORT_TERMS_ID")
	private Integer awardReportTermsId;

	@Column(name = "NOTIFY_DATE")
	private Timestamp notifyDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getAwardRepRecpntNotifyLogId() {
		return awardRepRecpntNotifyLogId;
	}

	public void setAwardRepRecpntNotifyLogId(Integer awardRepRecpntNotifyLogId) {
		this.awardRepRecpntNotifyLogId = awardRepRecpntNotifyLogId;
	}

	public Integer getAwardReportTermRecipientId() {
		return awardReportTermRecipientId;
	}

	public void setAwardReportTermRecipientId(Integer awardReportTermRecipientId) {
		this.awardReportTermRecipientId = awardReportTermRecipientId;
	}

	public Integer getAwardReportTermsId() {
		return awardReportTermsId;
	}

	public void setAwardReportTermsId(Integer awardReportTermsId) {
		this.awardReportTermsId = awardReportTermsId;
	}

	public Timestamp getNotifyDate() {
		return notifyDate;
	}

	public void setNotifyDate(Timestamp notifyDate) {
		this.notifyDate = notifyDate;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public AwardReportTermRecipient getReportTermRecipient() {
		return reportTermRecipient;
	}

	public void setReportTermRecipient(AwardReportTermRecipient reportTermRecipient) {
		this.reportTermRecipient = reportTermRecipient;
	}

}
