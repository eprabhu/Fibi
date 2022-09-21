package com.polus.fibicomp.general.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "DYN_SECTION_CONFIG")
public class DynamicSectionConfig implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SECTION_CODE")
	private String sectionCode;
	
	@Column(name = "MODULE_CODE")
	private String moduleCode;

	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@JsonManagedReference
	@OneToMany(mappedBy = "sectionConfig", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<DynamicSubSectionConfig> subSectionConfig;
}
