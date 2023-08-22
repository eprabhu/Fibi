import { Injectable } from '@angular/core';
import { EntityDetailsService } from './entity-details.service';

@Injectable()

export class EntityDetailsRouteGuardService {

constructor(private entityDetailsService: EntityDetailsService) { }

canDeactivate(): boolean {
  if (this.entityDetailsService.isRelationshipQuestionnaireChanged) {
      document.getElementById('hidden-unsaved-changes-button').click();
      return false;
  } else {
      return true;
  }
}
}
