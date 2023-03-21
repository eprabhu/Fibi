package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
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

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.pojo.RelevantField;

@Entity
@Table(name = "GRANT_CALL_RELEVANT_FIELD")
public class GrantCallRelevant implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "GRANT_CALL_RELEVANT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRANT_CALL_RELEVANT_ID_GENERATOR")
	@SequenceGenerator(name="GRANT_CALL_RELEVANT_ID_GENERATOR", sequenceName = "GRANT_CALL_RELEVANT_ID_GENERATOR", allocationSize=1)
	private Integer grantCallRelevantId;

	@Column(name = "RELEVANT_FIELD_CODE")
	private String relevantFieldCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_RELEVANT_FIELD_FK1"), name = "RELEVANT_FIELD_CODE", referencedColumnName = "RELEVANT_FIELD_CODE", insertable = false, updatable = false)
	private RelevantField relevantField;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_RELEVANT_FIELD_FK2"), name = "GRANT_HEADER_ID", referencedColumnName = "GRANT_HEADER_ID")
	private GrantCall grantCall;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getGrantCallRelevantId() {
		return grantCallRelevantId;
	}

	public void setGrantCallRelevantId(Integer grantCallRelevantId) {
		this.grantCallRelevantId = grantCallRelevantId;
	}

	public String getRelevantFieldCode() {
		return relevantFieldCode;
	}

	public void setRelevantFieldCode(String relevantFieldCode) {
		this.relevantFieldCode = relevantFieldCode;
	}

	public GrantCall getGrantCall() {
		return grantCall;
	}

	public void setGrantCall(GrantCall grantCall) {
		this.grantCall = grantCall;
	}

	public RelevantField getRelevantField() {
		return relevantField;
	}

	public void setRelevantField(RelevantField relevantField) {
		this.relevantField = relevantField;
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

}
