package com.polus.fibicomp.businessrule.metarule.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.pojo.Unit;

@Entity
@Table(name = "META_RULES")
@EntityListeners(AuditingEntityListener.class)
public class MetaRule implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "META_RULE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer metaRuleId;
	
	@Column(name = "UNIT_NUMBER")
	private String unitNumber;
	
	@Column(name = "META_RULE_TYPE")
	private String metaRuleType;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "SUB_MODULE_CODE")
	private Integer subModuleCode;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "META_RULES_FK1"), name = "UNIT_NUMBER", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

	@JsonManagedReference
    @OneToMany(mappedBy = "metaRule", cascade = CascadeType.ALL, fetch = FetchType.LAZY, orphanRemoval = true)
    private List<MetaRuleDetail> metaRuleDetails;

	public Integer getMetaRuleId() {
		return metaRuleId;
	}

	public void setMetaRuleId(Integer metaRuleId) {
		this.metaRuleId = metaRuleId;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public String getMetaRuleType() {
		return metaRuleType;
	}

	public void setMetaRuleType(String metaRuleType) {
		this.metaRuleType = metaRuleType;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
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

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public List<MetaRuleDetail> getMetaRuleDetails() {
		return metaRuleDetails;
	}

	public void setMetaRuleDetails(List<MetaRuleDetail> metaRuleDetails) {
		this.metaRuleDetails = metaRuleDetails;
	}

}
	