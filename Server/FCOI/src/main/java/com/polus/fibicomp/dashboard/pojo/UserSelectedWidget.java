package com.polus.fibicomp.dashboard.pojo;

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
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "USER_SELECTED_WIDGET",
uniqueConstraints = @UniqueConstraint(name = "USER_SELECTED_WIDGET_UK", columnNames = {"PERSON_ID", "WIDGET_ID"}))
@EntityListeners(AuditingEntityListener.class)

public class UserSelectedWidget implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "USER_SELECTED_WIDGET_ID_GENERATOR")
	@SequenceGenerator(name = "USER_SELECTED_WIDGET_ID_GENERATOR", sequenceName = "USER_SELECTED_WIDGET_ID_GENERATOR", allocationSize = 1)
	@Column(name = "SELECTED_WIDGET_ID")
	private Integer selectedWidgetId;

	@Column(name = "WIDGET_ID" , nullable = false, unique = true)
	private Integer widgetId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "USER_SELECTED_WIDGET_FK1"), name = "WIDGET_ID", referencedColumnName = "WIDGET_ID", insertable = false, updatable = false)
	private WidgetLookup widgetLookup;

	@Column(name = "SORT_ORDER")
	private Integer sortOrder;

	@Column(name = "PERSON_ID", nullable = false, unique = true)
	private String personId;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getSelectedWidgetId() {
		return selectedWidgetId;
	}

	public void setSelectedWidgetId(Integer selectedWidgetId) {
		this.selectedWidgetId = selectedWidgetId;
	}

	public Integer getWidgetId() {
		return widgetId;
	}

	public void setWidgetId(Integer widgetId) {
		this.widgetId = widgetId;
	}

	public WidgetLookup getWidgetLookup() {
		return widgetLookup;
	}

	public void setWidgetLookup(WidgetLookup widgetLookup) {
		this.widgetLookup = widgetLookup;
	}

	public Integer getSortOrder() {
		return sortOrder;
	}

	public void setSortOrder(Integer sortOrder) {
		this.sortOrder = sortOrder;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
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
