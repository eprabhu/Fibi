package com.polus.fibicomp.task.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

@Entity
@Table(name = "TASK_MODULE_MAPPING")
@IdClass(TaskModuleMapping.TaskModuleMappingId.class)
public class TaskModuleMapping implements Serializable, Comparable<TaskModuleMapping> {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "TASK_TYPE_CODE")
	private String taskTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "TASK_MODULE_MAPPING_FK"), name = "TASK_TYPE_CODE", referencedColumnName = "TASK_TYPE_CODE", insertable = false, updatable = false)
	private TaskType taskType;

	@Id
	@Column(name = "MODULE_CODE")
	private Integer moduleCode;	

	@Id
	@Column(name = "SUB_MODULE_CODE")
	private Integer subModuleCode;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getTaskTypeCode() {
		return taskTypeCode;
	}

	public void setTaskTypeCode(String taskTypeCode) {
		this.taskTypeCode = taskTypeCode;
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

	@Override
	public int compareTo(TaskModuleMapping taskModuleMapping) {
		return new CompareToBuilder().append(this.taskTypeCode, taskModuleMapping.taskTypeCode).append(this.moduleCode, taskModuleMapping.moduleCode).append(this.subModuleCode, taskModuleMapping.subModuleCode).toComparison();
	}

	public static final class TaskModuleMappingId implements Serializable, Comparable<TaskModuleMappingId>{

		private static final long serialVersionUID = 1L;

		private String taskTypeCode;

		private Integer moduleCode;

		private Integer subModuleCode;

		public String getTaskTypeCode() {
			return taskTypeCode;
		}

		public void setTaskTypeCode(String taskTypeCode) {
			this.taskTypeCode = taskTypeCode;
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

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("taskTypeCode", this.taskTypeCode).append("moduleCode", this.moduleCode).append("subModuleCode", this.subModuleCode).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final TaskModuleMappingId rhs = (TaskModuleMappingId) other;
			return new EqualsBuilder().append(this.taskTypeCode, rhs.taskTypeCode).append(this.moduleCode, rhs.moduleCode).append(this.subModuleCode, rhs.subModuleCode).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.taskTypeCode).append(this.moduleCode).append(this.subModuleCode).toHashCode();
		}

		@Override
		public int compareTo(TaskModuleMappingId other) {
			return new CompareToBuilder().append(this.taskTypeCode, other.taskTypeCode).append(this.moduleCode, other.moduleCode).append(this.subModuleCode, other.subModuleCode).toComparison();
		}
	}

}
