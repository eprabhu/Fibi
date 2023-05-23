package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "DISCL_FILE_DATA")
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DisclFileData implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "FILE_DATA_ID")
	private String fileDataId;

	@Column(name = "FILE_PATH")
	private String filePath;
	 
    @Column(name = "ORIGINAL_FILE_NAME")
    private String originalFileName;
    
    @Column(name = "FILE_NAME")
    private String fileName;
	
}
