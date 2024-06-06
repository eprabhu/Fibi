package com.polus.formbuilder.customdataelement.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;

import lombok.Data;

@Entity(name = "fbCompCustomElementOptions")
@Data
@Table(name = "FB_COMP_CUSTOM_ELEMENT_OPTIONS")
public class CustomDataElementOption implements Serializable{

	private static final long serialVersionUID = 1L;
	
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "CUSTOM_DATA_OPTION_ID")
	private Integer customDataOptionId;
	
	@Column(name = "CUSTOM_DATA_ELEMENTS_ID")
	private Integer customDataElementsId;
	
	@Column(name = "OPTION_NAME")
	private String optionName;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Transient
	private String acType;

}
