package com.polus.fibicomp.coi.pojo;

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

import com.polus.fibicomp.pojo.Sponsor;

@Entity
@Table(name = "ENTITY_SPONSOR_RELATIONSHIP")
@EntityListeners(AuditingEntityListener.class)
public class EntitySponsorRelationship implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ENTITY_SPONSOR_RELATIONSHIP_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer entitySPonsorRelationshipId;

	@Column(name = "COI_ENTITY_ID")
	private Integer coiEntityId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_SPONSOR_RELATIONSHIP_FK1"), name = "COI_ENTITY_ID", referencedColumnName = "COI_ENTITY_ID", insertable = false, updatable = false)
	private COIEntity coiEntity;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_SPONSOR_RELATIONSHIP_FK2"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor sponsor;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getEntitySPonsorRelationshipId() {
		return entitySPonsorRelationshipId;
	}

	public void setEntitySPonsorRelationshipId(Integer entitySPonsorRelationshipId) {
		this.entitySPonsorRelationshipId = entitySPonsorRelationshipId;
	}

	public Integer getCoiEntityId() {
		return coiEntityId;
	}

	public void setCoiEntityId(Integer coiEntityId) {
		this.coiEntityId = coiEntityId;
	}

	public COIEntity getCoiEntity() {
		return coiEntity;
	}

	public void setCoiEntity(COIEntity coiEntity) {
		this.coiEntity = coiEntity;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
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
