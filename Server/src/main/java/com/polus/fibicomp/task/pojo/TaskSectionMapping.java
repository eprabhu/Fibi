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

import com.polus.fibicomp.sectionwiseedit.pojo.SectionType;

@Entity
@Table(name = "TASK_SECTION_MAPPING")
@IdClass(TaskSectionMapping.TaskSectionMappingId.class)
public class TaskSectionMapping implements Serializable, Comparable<TaskSectionMapping> {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "TASK_TYPE_CODE")
	private String taskTypeCode;

	@Id
	@Column(name = "SECTION_CODE")
	private String sectionCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "TASK_SECTION_MAPPING_FK1"), name = "TASK_TYPE_CODE", referencedColumnName = "TASK_TYPE_CODE", insertable = false, updatable = false)
	private TaskType taskType;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "TASK_SECTION_MAPPING_FK2"), name = "SECTION_CODE", referencedColumnName = "SECTION_CODE", insertable = false, updatable = false)
	private SectionType sectionType;

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

	public TaskType getTaskType() {
		return taskType;
	}

	public void setTaskType(TaskType taskType) {
		this.taskType = taskType;
	}

	public String getSectionCode() {
		return sectionCode;
	}

	public void setSectionCode(String sectionCode) {
		this.sectionCode = sectionCode;
	}

	public SectionType getSectionType() {
		return sectionType;
	}

	public void setSectionType(SectionType sectionType) {
		this.sectionType = sectionType;
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
	public int compareTo(TaskSectionMapping tasksectionMapping) {
		return new CompareToBuilder().append(this.taskTypeCode, tasksectionMapping.taskTypeCode).append(this.sectionCode, tasksectionMapping.sectionCode).toComparison();
	}

	public static final class TaskSectionMappingId implements Serializable, Comparable<TaskSectionMappingId>{

		private static final long serialVersionUID = 1L;

		private String taskTypeCode;

		private String sectionCode;

		public String getTaskTypeCode() {
			return taskTypeCode;
		}

		public void setTaskTypeCode(String taskTypeCode) {
			this.taskTypeCode = taskTypeCode;
		}

		public String getSectionCode() {
			return sectionCode;
		}

		public void setSectionCode(String sectionCode) {
			this.sectionCode = sectionCode;
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("taskTypeCode", this.taskTypeCode).append("sectionCode", this.sectionCode).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final TaskSectionMappingId rhs = (TaskSectionMappingId) other;
			return new EqualsBuilder().append(this.taskTypeCode, rhs.taskTypeCode).append(this.sectionCode, rhs.sectionCode).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.taskTypeCode).append(this.sectionCode).toHashCode();
		}

		@Override
		public int compareTo(TaskSectionMappingId other) {
			return new CompareToBuilder().append(this.taskTypeCode, other.taskTypeCode).append(this.sectionCode, other.sectionCode).toComparison();
		}
	}

}
