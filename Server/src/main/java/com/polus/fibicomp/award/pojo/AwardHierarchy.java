package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "AWARD_HIERARCHY")
public class AwardHierarchy implements Serializable {


	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_HIERARCHY_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_HIERARCHY_ID_GENERATOR")
	@SequenceGenerator(name="AWARD_HIERARCHY_ID_GENERATOR", sequenceName = "AWARD_HIERARCHY_ID_GENERATOR", allocationSize=1)
	private Integer awardHierarchyId;
	
	@Column(name= "ROOT_AWARD_NUMBER")
	private String rootAwardNumber;
	
	@Column(name= "AWARD_NUMBER")
	private String awardNumber;
	
	@Column(name= "PARENT_AWARD_NUMBER")
	private String parentAwardNumber;
	
	@Column(name= "ORIGINATING_AWARD_NUMBER")
	private String originatingAwardNumber;
	
	@Column(name= "ACTIVE")
	private String active;
	
	@Column(name = "UPDATE_USER")
	private String createUser;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public Integer getAwardHierarchyId() {
		return awardHierarchyId;
	}

	public void setAwardHierarchyId(Integer awardHierarchyId) {
		this.awardHierarchyId = awardHierarchyId;
	}

	public String getRootAwardNumber() {
		return rootAwardNumber;
	}

	public void setRootAwardNumber(String rootAwardNumber) {
		this.rootAwardNumber = rootAwardNumber;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getParentAwardNumber() {
		return parentAwardNumber;
	}

	public void setParentAwardNumber(String parentAwardNumber) {
		this.parentAwardNumber = parentAwardNumber;
	}

	public String getOriginatingAwardNumber() {
		return originatingAwardNumber;
	}

	public void setOriginatingAwardNumber(String originatingAwardNumber) {
		this.originatingAwardNumber = originatingAwardNumber;
	}

	public String getActive() {
		return active;
	}

	public void setActive(String active) {
		this.active = active;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}
		
}
