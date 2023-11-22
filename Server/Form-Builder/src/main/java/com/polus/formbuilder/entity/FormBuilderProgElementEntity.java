package com.polus.formbuilder.entity;

import java.io.Serializable;
import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "FORM_BUILDER_PROG_ELEMENT")
public class FormBuilderProgElementEntity implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "PROG_ELEMENT_ID")
    private Integer progElementId;

    @Column(name = "PROG_ELEMENT_NUMBER", nullable = false)
    private String progElementNumber;

    @Column(name = "VERSION_NUMBER", nullable = false)
    private Integer versionNumber;

    @Column(name = "VERSION_STATUS", nullable = false)
    private String versionStatus;

    @Column(name = "PROG_ELEMENT_NAME", nullable = false)
    private String progElementName;

    @Column(name = "DESCRIPTION", nullable = false)
    private String description;

    @Column(name = "IS_ACTIVE", nullable = false)
    private String isActive;

    @Column(name = "UPDATE_TIMESTAMP", nullable = false)
    @Temporal(TemporalType.TIMESTAMP)
    private Date updateTimestamp;

    @Column(name = "UPDATE_USER", nullable = false)
    private String updateUser;
    
}