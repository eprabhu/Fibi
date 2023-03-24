package com.polus.fibicomp.ip.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "PROPOSAL_ACTION_TYPE")
public class InstituteProposalActionType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ACTION_TYPE_CODE")
	private String actionTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Column(name = "STATUS_CODE")
	private Integer statusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_ACTION_TYPE_FK1"), name = "STATUS_CODE", referencedColumnName = "STATUS_CODE", insertable = false, updatable = false)
	private InstituteProposalStatus instProposalStatus;

	@Column(name = "ACTION_ITEM_ID")
	private String actionItemId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_ACTION_TYPE_FK2"), name = "ACTION_ITEM_ID", referencedColumnName = "ACTION_ITEM_ID", insertable = false, updatable = false)
	private InstituteProposalActionItem instituteProposalActionItem;

	public String getActionTypeCode() {
		return actionTypeCode;
	}

	public void setActionTypeCode(String actionTypeCode) {
		this.actionTypeCode = actionTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

	public Integer getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(Integer statusCode) {
		this.statusCode = statusCode;
	}

	public InstituteProposalStatus getInstProposalStatus() {
		return instProposalStatus;
	}

	public void setInstProposalStatus(InstituteProposalStatus instProposalStatus) {
		this.instProposalStatus = instProposalStatus;
	}

	public String getActionItemId() {
		return actionItemId;
	}

	public void setActionItemId(String actionItemId) {
		this.actionItemId = actionItemId;
	}

	public InstituteProposalActionItem getInstituteProposalActionItem() {
		return instituteProposalActionItem;
	}

	public void setInstituteProposalActionItem(InstituteProposalActionItem instituteProposalActionItem) {
		this.instituteProposalActionItem = instituteProposalActionItem;
	}

}
