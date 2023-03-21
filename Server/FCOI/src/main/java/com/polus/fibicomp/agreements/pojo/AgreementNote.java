package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name = "AGREEMENT_NOTE")
public class AgreementNote implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AGREEMENT_NOTE")
	@SequenceGenerator(name="SEQ_AGREEMENT_NOTE", sequenceName = "SEQ_AGREEMENT_NOTE", allocationSize=1)
	@Column(name = "AGREEMENT_NOTE_ID")
	private Integer agreementNoteId;

	@Column(name = "AGREEMENT_REQUEST_ID")
	private Integer agreementRequestId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_NOTE_FK1"), name = "AGREEMENT_REQUEST_ID", referencedColumnName = "AGREEMENT_REQUEST_ID", insertable = false, updatable = false)
	private AgreementHeader agreementHeader;

	@Column(name = "ACTION_LOG_ID")
	private Integer actionLogId;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_NOTE_FK2"), name = "ACTION_LOG_ID", referencedColumnName = "ACTION_LOG_ID", insertable = false, updatable = false)
	private AgreementActionLog agreementActionLog;

	@Column(name = "NOTE")
	private String note;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "agreementNote", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<AgreementNoteAttachment> agreementNoteAttachment;

	@Transient
	private String updateUserFullName;

	public AgreementNote() {
		agreementNoteAttachment = new ArrayList<>();
	}

	public Integer getAgreementNoteId() {
		return agreementNoteId;
	}

	public void setAgreementNoteId(Integer agreementNoteId) {
		this.agreementNoteId = agreementNoteId;
	}

	public Integer getAgreementRequestId() {
		return agreementRequestId;
	}

	public void setAgreementRequestId(Integer agreementRequestId) {
		this.agreementRequestId = agreementRequestId;
	}

	public AgreementHeader getAgreementHeader() {
		return agreementHeader;
	}

	public void setAgreementHeader(AgreementHeader agreementHeader) {
		this.agreementHeader = agreementHeader;
	}

	public Integer getActionLogId() {
		return actionLogId;
	}

	public void setActionLogId(Integer actionLogId) {
		this.actionLogId = actionLogId;
	}

	public AgreementActionLog getAgreementActionLog() {
		return agreementActionLog;
	}

	public void setAgreementActionLog(AgreementActionLog agreementActionLog) {
		this.agreementActionLog = agreementActionLog;
	}

	public String getNote() {
		return note;
	}

	public void setNote(String note) {
		this.note = note;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public List<AgreementNoteAttachment> getAgreementNoteAttachment() {
		return agreementNoteAttachment;
	}

	public void setAgreementNoteAttachment(List<AgreementNoteAttachment> agreementNoteAttachment) {
		this.agreementNoteAttachment = agreementNoteAttachment;
	}

}
