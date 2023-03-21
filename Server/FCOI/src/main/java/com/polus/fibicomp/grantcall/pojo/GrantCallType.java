package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "GRANT_CALL_TYPE")
public class GrantCallType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "grantTypeCodeGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "grantTypeCodeGenerator")
	@Column(name = "GRANT_TYPE_CODE")
	private Integer grantTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "GRANT_CALL_CATEGORY_CODE")
	private Integer categoryCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_TYPE_FK1"), name = "GRANT_CALL_CATEGORY_CODE", referencedColumnName = "GRANT_CALL_CATEGORY_CODE", insertable = false, updatable = false)
	private GrantCallCategory grantCallCategory;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	public Integer getGrantTypeCode() {
		return grantTypeCode;
	}

	public void setGrantTypeCode(Integer grantTypeCode) {
		this.grantTypeCode = grantTypeCode;
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

	public Integer getCategoryCode() {
		return categoryCode;
	}

	public void setCategoryCode(Integer categoryCode) {
		this.categoryCode = categoryCode;
	}

	public GrantCallCategory getGrantCallCategory() {
		return grantCallCategory;
	}

	public void setGrantCallCategory(GrantCallCategory grantCallCategory) {
		this.grantCallCategory = grantCallCategory;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

}
