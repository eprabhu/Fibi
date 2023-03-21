package com.polus.fibicomp.grantcall.pojo;

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
@Table(name = "GRANT_SCORING_CRITERIA")
public class GrantCallScoringCriteria implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "GRANT_SCORE_CRITERIA_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRANT_SCORING_CRITERIA_ID_GENERATOR")
	@SequenceGenerator(name="GRANT_SCORING_CRITERIA_ID_GENERATOR", sequenceName = "GRANT_SCORING_CRITERIA_ID_GENERATOR", allocationSize=1)
	private Integer grantScoreCriteriaId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_SCORING_CRITERIA_FK2"), name = "GRANT_HEADER_ID", referencedColumnName = "GRANT_HEADER_ID", insertable = false, updatable = false)
	private GrantCall grantCall;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_SCORING_CRITERIA_FK1"), name = "SCORING_CRITERIA_TYPE_CODE", referencedColumnName = "SCORING_CRITERIA_TYPE_CODE", insertable = false, updatable = false)
	private ScoringCriteria scoringCriteria;

	@Column(name = "GRANT_HEADER_ID")
	private Integer grantCallId;

	@Column(name = "SCORING_CRITERIA_TYPE_CODE")
	private String scoringCriteriaTypeCode;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getGrantScoreCriteriaId() {
		return grantScoreCriteriaId;
	}

	public void setGrantScoreCriteriaId(Integer grantScoreCriteriaId) {
		this.grantScoreCriteriaId = grantScoreCriteriaId;
	}

	public GrantCall getGrantCall() {
		return grantCall;
	}

	public void setGrantCall(GrantCall grantCall) {
		this.grantCall = grantCall;
	}

	public ScoringCriteria getScoringCriteria() {
		return scoringCriteria;
	}

	public void setScoringCriteria(ScoringCriteria scoringCriteria) {
		this.scoringCriteria = scoringCriteria;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public String getScoringCriteriaTypeCode() {
		return scoringCriteriaTypeCode;
	}

	public void setScoringCriteriaTypeCode(String scoringCriteriaTypeCode) {
		this.scoringCriteriaTypeCode = scoringCriteriaTypeCode;
	}

}
