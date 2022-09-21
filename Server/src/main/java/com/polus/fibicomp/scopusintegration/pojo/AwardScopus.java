package com.polus.fibicomp.scopusintegration.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "AWARD_SCOPUS")
@EntityListeners(AuditingEntityListener.class)
public class AwardScopus implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_SCOPUS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardScopusId;

	@Column(name = "SCOPUS_ID")
	private String scopusId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_SCOPUS_FK1"), name = "SCOPUS_ID", referencedColumnName = "SCOPUS_ID", insertable = false, updatable = false)
	private Scopus scopus;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getAwardScopusId() {
		return awardScopusId;
	}

	public void setAwardScopusId(Integer awardScopusId) {
		this.awardScopusId = awardScopusId;
	}

	public String getScopusId() {
		return scopusId;
	}

	public void setScopusId(String scopusId) {
		this.scopusId = scopusId;
	}

	public Scopus getScopus() {
		return scopus;
	}

	public void setScopus(Scopus scopus) {
		this.scopus = scopus;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
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
