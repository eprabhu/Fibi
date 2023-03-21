package com.polus.fibicomp.customdataelement.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "CUSTOM_DATA_ELEMENTS")
public class CustomDataElements implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "customDataElementIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "customDataElementIdGenerator")
	@Column(name = "CUSTOM_DATA_ELEMENTS_ID")
	private Integer customElementId;

	@Column(name = "COLUMN_ID")
	private Integer columnId;

	@Column(name = "COLUMN_VERSION_NUMBER")
	private Integer columnVersionNumber;

	@Column(name = "COLUMN_LABEL")
	private String columnLabel;

	@Column(name = "DATA_TYPE", length = 3)
	private String dataType;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "CUSTOM_DATA_ELEMENTS_FK1"), name = "DATA_TYPE", referencedColumnName = "DATA_TYPE_CODE", insertable = false, updatable = false)
	private CustomDataElementDataType customDataTypes;

	@Column(name = "DATA_LENGTH")
	private Integer dataLength;

	@Column(name = "DEFAULT_VALUE")
	private String defaultValue;

	@Column(name = "IS_LATEST_VERSION")
	private String isLatestVesrion;

	@Column (name = "HAS_LOOKUP")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean hasLookup;

	@Column (name = "LOOKUP_WINDOW")
	private String lookupWindow;

	@Column (name = "LOOKUP_ARGUMENT")
	private String lookupArgument;

	@Column (name = "IS_ACTIVE")
	private String isActive;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "CUSTOM_ELEMENT_NAME")
	private String customElementName;

	@JsonManagedReference
	@OneToMany(mappedBy = "customDataElement",orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<CustomDataElementUsage> customDataElementUsage;

	@Transient
	private String acType;

	public CustomDataElements() {
		customDataElementUsage = new ArrayList<>();
	}

	public Integer getCustomElementId() {
		return customElementId;
	}

	public void setCustomElementId(Integer customElementId) {
		this.customElementId = customElementId;
	}

	public Integer getColumnId() {
		return columnId;
	}

	public void setColumnId(Integer columnId) {
			this.columnId = columnId;
	}

	public Integer getColumnVersionNumber() {
		return columnVersionNumber;
	}

	public void setColumnVersionNumber(Integer columnVersionNumber) {
		this.columnVersionNumber = columnVersionNumber;
	}

	public String getColumnLabel() {
		return columnLabel;
	}

	public void setColumnLabel(String columnLabel) {
		this.columnLabel = columnLabel;
	}

	public String getDataType() {
		return dataType;
	}

	public void setDataType(String dataType) {
		this.dataType = dataType;
	}

	public Integer getDataLength() {
		return dataLength;
	}

	public void setDataLength(Integer dataLength) {
		this.dataLength = dataLength;
	}

	public String getDefaultValue() {
		return defaultValue;
	}

	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}

	public String getIsLatestVesrion() {
		return isLatestVesrion;
	}

	public void setIsLatestVesrion(String isLatestVesrion) {
		this.isLatestVesrion = isLatestVesrion;
	}

	public Boolean getHasLookup() {
		return hasLookup;
	}

	public void setHasLookup(Boolean hasLookup) {
		this.hasLookup = hasLookup;
	}

	public String getLookupWindow() {
		return lookupWindow;
	}

	public void setLookupWindow(String lookupWindow) {
		this.lookupWindow = lookupWindow;
	}

	public String getLookupArgument() {
		return lookupArgument;
	}

	public void setLookupArgument(String lookupArgument) {
		this.lookupArgument = lookupArgument;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
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

	public List<CustomDataElementUsage> getCustomDataElementUsage() {
		return customDataElementUsage;
	}

	public void setCustomDataElementUsage(List<CustomDataElementUsage> customDataElementUsage) {
		this.customDataElementUsage = customDataElementUsage;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public CustomDataElementDataType getCustomDataTypes() {
		return customDataTypes;
	}

	public void setCustomDataTypes(CustomDataElementDataType customDataTypes) {
		this.customDataTypes = customDataTypes;
	}

	public String getCustomElementName() {
		return customElementName;
	}

	public void setCustomElementName(String customElementName) {
		this.customElementName = customElementName;
	}
}
