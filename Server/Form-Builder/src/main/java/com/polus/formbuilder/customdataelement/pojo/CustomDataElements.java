package com.polus.formbuilder.customdataelement.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.JoinColumn;

import com.polus.formbuilder.entity.FormBuilderComponentTypeEntity;

import jakarta.persistence.ManyToOne;
import lombok.Data;

@Entity(name = "fbCompCustomElement")
@Data
@Table(name = "FB_COMP_CUSTOM_ELEMENT")
public class CustomDataElements implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "CUSTOM_DATA_ELEMENTS_ID")
	private Integer customElementId;

	@Column(name = "COLUMN_LABEL")
	private String columnLabel;

	@Column(name = "DATA_TYPE", length = 3)
	private String dataType;

	@ManyToOne(optional = true, cascade = CascadeType.ALL)
	@JoinColumn(foreignKey = @ForeignKey(name = "FB_COMP_CUSTOM_ELEMENT_FK1"), name = "DATA_TYPE", referencedColumnName = "COMPONENT_TYPE_CODE", insertable = false, updatable = false)
	private FormBuilderComponentTypeEntity customDataTypes;

	@Column(name = "DATA_LENGTH")
	private Integer dataLength;

	@Column(name = "DEFAULT_VALUE")
	private String defaultValue;

	@Column(name = "IS_MULTI_SELECT_LOOKUP")
	private String isMultiSelectLookup;

	@Column(name = "HAS_LOOKUP")
	private String hasLookup;

	@Column(name = "LOOKUP_WINDOW")
	private String lookupWindow;

	@Column(name = "LOOKUP_ARGUMENT")
	private String lookupArgument;

	@Column(name = "IS_ACTIVE")
	private String isActive;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "CUSTOM_ELEMENT_NAME")
	private String customElementName;

	@Transient
	private String acType;

}
