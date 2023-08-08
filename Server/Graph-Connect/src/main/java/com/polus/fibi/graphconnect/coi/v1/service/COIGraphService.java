package com.polus.fibi.graphconnect.coi.v1.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.fibi.graphconnect.coi.entity.COIEntity;
import com.polus.fibi.graphconnect.coi.repository.EntityRepository;
import com.polus.fibi.graphconnect.coi.v1.dao.COIEntityGraphDao;
import com.polus.fibi.graphconnect.coi.v1.dao.COIImportGraphDataDao;
import com.polus.fibi.graphconnect.model.RequestDTO;
import com.polus.fibi.graphconnect.model.ResponseDTO;

@Service
public class COIGraphService {

	@Autowired
	private EntityRepository entityRepository;

	@Autowired
	private COIEntityGraphDao coiEntityGraphDao;
	
	@Autowired
	private COIImportGraphDataDao coiImportGraphDataDao;
	

    public List<COIEntity> getAllEntity() {
        return entityRepository.findAll();
    }    	
	   
	public void importDataFromRDBMS() {
		coiImportGraphDataDao.importDataFromRDBMS();		
	}
	public void refreshDataFromRDBMS() {
		coiImportGraphDataDao.refreshDataFromRDBMS();		
	}
		
	public ResponseDTO entityGraph(RequestDTO request) { 
		return coiEntityGraphDao.entityGraphDAO(request);
	}	
	
}
