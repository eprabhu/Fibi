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

import com.polus.fibicomp.pojo.Organization;

@Entity
@Table(name = "ENTITY_ORG_RELATIONSHIP")
@EntityListeners(AuditingEntityListener.class)
public class EntityOrgRelationship implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ENTITY_ORG_RELATIONSHIP_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer entityOrgRelationshipId;

	@Column(name = "COI_ENTITY_ID")
	private Integer coiEntityId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_ORG_RELATIONSHIP_FK1"), name = "COI_ENTITY_ID", referencedColumnName = "COI_ENTITY_ID", insertable = false, updatable = false)
	private COIEntity coiEntity;

	@Column(name = "ORGANIZATION_ID")
	private String organizationId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_ORG_RELATIONSHIP_FK2"), name = "ORGANIZATION_ID", referencedColumnName = "ORGANIZATION_ID", insertable = false, updatable = false)
	private Organization organization;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getEntityOrgRelationshipId() {
		return entityOrgRelationshipId;
	}

	public void setEntityOrgRelationshipId(Integer entityOrgRelationshipId) {
		this.entityOrgRelationshipId = entityOrgRelationshipId;
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

	public String getOrganizationId() {
		return organizationId;
	}

	public void setOrganizationId(String organizationId) {
		this.organizationId = organizationId;
	}

	public Organization getOrganization() {
		return organization;
	}

	public void setOrganization(Organization organization) {
		this.organization = organization;
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
