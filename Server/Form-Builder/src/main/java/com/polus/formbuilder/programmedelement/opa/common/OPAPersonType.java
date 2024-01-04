package com.polus.formbuilder.programmedelement.opa.common;

import java.io.Serializable;
import java.sql.Timestamp;

import jakarta.persistence.Column;
import jakarta.persistence.Id;

import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;

@Entity
@Data
@Table(name = "OPA_PERSON_TYPE")
public class OPAPersonType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "OPA_PERSON_TYPE_CODE")
	private String opaPersonTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	private String isActive;

}
