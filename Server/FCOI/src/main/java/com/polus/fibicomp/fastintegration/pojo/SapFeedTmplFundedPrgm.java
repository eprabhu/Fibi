package com.polus.fibicomp.fastintegration.pojo;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "SAP_FEED_TMPL_FUNDED_PRGM")
@EntityListeners(AuditingEntityListener.class)
public class SapFeedTmplFundedPrgm {

	@Id
	@Column(name = "ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SAP_FEED_TMPL_FUNDED_PRGM_ID_GENERATOR")
	@SequenceGenerator(name = "SAP_FEED_TMPL_FUNDED_PRGM_ID_GENERATOR", sequenceName = "SAP_FEED_TMPL_FUNDED_PRGM_ID_GENERATOR", allocationSize = 1)
	private Integer id;

	@Column(name = "BATCH_ID")
	private Integer batchId;

	@Column(name = "FEED_ID")
	private Integer feedId;

	@Column(name = "FUNDED_PROGRAM")
	private String fundedProgram;
	
	@Column(name = "PROGRAM_DESCRIPTION")
	private String programDescription;

	@Column(name = "FUNDED_PROGRAM_TYPE")
	private String fundedProgramType;

	@Column(name = "CREATE_STATUS")
	private String createStatus;

	@Column(name = "FEED_STATUS")
	private String feedStatus;

	@Column(name = "ERROR_MESSAGE")
	private String errorMessage;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "USER_COMMENT")
	private String userComment;

	public SapFeedTmplFundedPrgm() {}
	
	public SapFeedTmplFundedPrgm(String fundedProgram, Integer batchId, String feedStatus, String message) {
		this.fundedProgram = fundedProgram;
//		this.programDescription = programDescription;
//		this.fundedProgramType = fundedProgramType;
		this.batchId = batchId;
		this.feedStatus = feedStatus;
		this.errorMessage = message;
	}

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public Integer getBatchId() {
		return batchId;
	}

	public void setBatchId(Integer batchId) {
		this.batchId = batchId;
	}

	public Integer getFeedId() {
		return feedId;
	}

	public void setFeedId(Integer feedId) {
		this.feedId = feedId;
	}

	public String getFundedProgram() {
		return fundedProgram;
	}

	public void setFundedProgram(String fundedProgram) {
		this.fundedProgram = fundedProgram;
	}

	public String getProgramDescription() {
		return programDescription;
	}

	public void setProgramDescription(String programDescription) {
		this.programDescription = programDescription;
	}

	public String getFundedProgramType() {
		return fundedProgramType;
	}

	public void setFundedProgramType(String fundedProgramType) {
		this.fundedProgramType = fundedProgramType;
	}

	public String getCreateStatus() {
		return createStatus;
	}

	public void setCreateStatus(String createStatus) {
		this.createStatus = createStatus;
	}

	public String getFeedStatus() {
		return feedStatus;
	}

	public void setFeedStatus(String feedStatus) {
		this.feedStatus = feedStatus;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getUserComment() {
		return userComment;
	}

	public void setUserComment(String userComment) {
		this.userComment = userComment;
	}
	
}
