package com.polus.fibicomp.authorization.document;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Transactional
@Service(value = "userDocumentAuthorization")
public interface UserDocumentAuthorization {

	public boolean isAuthorized(Integer moduleCode,String moduleItemKey,String loggedInPerson);
	
}
