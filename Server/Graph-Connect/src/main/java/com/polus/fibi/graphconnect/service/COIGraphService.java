package com.polus.fibi.graphconnect.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.fibi.graphconnect.dao.COIEntityGraphDao;
import com.polus.fibi.graphconnect.dao.COIImportGraphDataDao;
import com.polus.fibi.graphconnect.entity.COIEntity;
import com.polus.fibi.graphconnect.model.RequestDTO;
import com.polus.fibi.graphconnect.model.ResponseDTO;
import com.polus.fibi.graphconnect.repository.EntityRepository;

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
