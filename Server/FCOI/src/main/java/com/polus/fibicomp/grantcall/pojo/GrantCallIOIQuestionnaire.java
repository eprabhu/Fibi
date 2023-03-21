package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "GRANT_CALL_IOI_QUESTIONNAIRE")
public class GrantCallIOIQuestionnaire implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "grantIOIQuestionnaireIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "grantIOIQuestionnaireIdGenerator")
	@Column(name = "IOI_QUESTIONNAIRE_ID")
	private Integer grantIOIQuestionnaireId;

	@Column(name = "GRANT_HEADER_ID")
	private Integer grantCallId;

	@JsonBackReference
	@OneToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_IOI_QNR_FK"), name = "GRANT_HEADER_ID", referencedColumnName = "GRANT_HEADER_ID", insertable = false, updatable = false)
	private GrantCall grantCall;

	@Column(name = "QUESTIONNAIRE_ID")
	private Integer questionnaireId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getGrantIOIQuestionnaireId() {
		return grantIOIQuestionnaireId;
	}

	public void setGrantIOIQuestionnaireId(Integer grantIOIQuestionnaireId) {
		this.grantIOIQuestionnaireId = grantIOIQuestionnaireId;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public GrantCall getGrantCall() {
		return grantCall;
	}

	public void setGrantCall(GrantCall grantCall) {
		this.grantCall = grantCall;
	}

	public Integer getQuestionnaireId() {
		return questionnaireId;
	}

	public void setQuestionnaireId(Integer questionnaireId) {
		this.questionnaireId = questionnaireId;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

}
