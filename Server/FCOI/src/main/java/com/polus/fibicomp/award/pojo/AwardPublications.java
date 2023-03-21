package com.polus.fibicomp.award.pojo;

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

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "AWARD_PUBLICATIONS")
@EntityListeners(AuditingEntityListener.class)
public class AwardPublications implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_PUBLICATION_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardPublicationId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "PUBLICATION_ID")
	private Integer publicationId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_PUBLICATIONS_FK1"), name = "PUBLICATION_ID", referencedColumnName = "PUBLICATION_ID", insertable = false, updatable = false)
	private Publication publication;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getAwardPublicationId() {
		return awardPublicationId;
	}

	public void setAwardPublicationId(Integer awardPublicationId) {
		this.awardPublicationId = awardPublicationId;
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

	public Integer getPublicationId() {
		return publicationId;
	}

	public void setPublicationId(Integer publicationId) {
		this.publicationId = publicationId;
	}

	public Publication getPublication() {
		return publication;
	}

	public void setPublication(Publication publication) {
		this.publication = publication;
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

}
