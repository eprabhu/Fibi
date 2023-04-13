package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.ForeignKey;

@Entity
@Table(name = "ENTITY_RELATIONSHIP")
public class EntityRelationship implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ID")
	private Integer id;
	
	@Column(name = "ENTITY_NUMBER")
	private Integer entityNumber;

	@Column(name = "NODE_TYPE_CODE")
	private Integer nodeTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_RELATIONSHIP_FK1"), name = "NODE_TYPE_CODE", referencedColumnName = "REL_NODE_TYPE_CODE", insertable = false, updatable = false)
	private EntityRelNodeType entityRelNodeType;
	
	@Column(name = "NODE_ID")
	private Integer nodeId;
	
	@Column(name = "ENTITY_REL_TYPE_CODE")
	private Integer entityRelTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_RELATIONSHIP_FK2"), name = "ENTITY_REL_TYPE_CODE", referencedColumnName = "ENTITY_REL_TYPE_CODE", insertable = false, updatable = false)
	private EntityRelationshipType entityRelationshipType;
	
	@Column(name = "ENTITY_ID")
	private Integer entityId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

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
