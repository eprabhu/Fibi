package com.polus.fibicomp.businessrule.metarule.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
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

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "META_RULE_DETAIL")
@EntityListeners(AuditingEntityListener.class)
public class MetaRuleDetail implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "META_RULE_DETAIL_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer metaRuleDetailId;
	
	@Column(name = "NODE_NUMBER")
	private Integer nodeNumber;
	
	@Column(name = "RULE_ID")
	private Integer ruleId;

	@Column(name = "PARENT_NODE")
	private Integer parentNode;

	@Column(name = "NEXT_NODE")
	private Integer nextNode;

	@Column(name = "NODE_IF_TRUE")
	private Integer nodeIfTrue;

	@Column(name = "NODE_IF_FALSE")
	private Integer nodeIfFalse;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "META_RULE_DETAIL_FK1"), name = "META_RULE_ID", referencedColumnName = "META_RULE_ID")
	private MetaRule metaRule;

	public Integer getMetaRuleDetailId() {
		return metaRuleDetailId;
	}

	public void setMetaRuleDetailId(Integer metaRuleDetailId) {
		this.metaRuleDetailId = metaRuleDetailId;
	}

	public Integer getNodeNumber() {
		return nodeNumber;
	}

	public void setNodeNumber(Integer nodeNumber) {
		this.nodeNumber = nodeNumber;
	}

	public Integer getRuleId() {
		return ruleId;
	}

	public void setRuleId(Integer ruleId) {
		this.ruleId = ruleId;
	}

	public Integer getParentNode() {
		return parentNode;
	}

	public void setParentNode(Integer parentNode) {
		this.parentNode = parentNode;
	}

	public Integer getNextNode() {
		return nextNode;
	}

	public void setNextNode(Integer nextNode) {
		this.nextNode = nextNode;
	}

	public Integer getNodeIfTrue() {
		return nodeIfTrue;
	}

	public void setNodeIfTrue(Integer nodeIfTrue) {
		this.nodeIfTrue = nodeIfTrue;
	}

	public Integer getNodeIfFalse() {
		return nodeIfFalse;
	}

	public void setNodeIfFalse(Integer nodeIfFalse) {
		this.nodeIfFalse = nodeIfFalse;
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

	public MetaRule getMetaRule() {
		return metaRule;
	}

	public void setMetaRule(MetaRule metaRule) {
		this.metaRule = metaRule;
	}

}
	