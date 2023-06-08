import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { EntityDetailsService } from './entity-details.service';
import { scrollIntoView } from '../../../../../fibi/src/app/common/utilities/custom-utilities';

@Component({
  selector: 'app-entity-details',
  templateUrl: './entity-details.component.html',
  styleUrls: ['./entity-details.component.scss']
})
export class EntityDetailsComponent implements  OnInit, OnDestroy {

  constructor(public entityDetailService: EntityDetailsService, private _route: ActivatedRoute) {
    this.clearSfiNavBarStyle();
  }
  updateRelationshipDetails: any;
  isEnableRelationshipModal = false;

  ngOnInit() {
    const isEditMode = this._route.snapshot.queryParamMap.get('mode') === 'edit';
    if (isEditMode) {
      this.entityDetailService.isExpanded = false;
    }
  }

  ngOnDestroy(): void {
    this.entityDetailService.isExpanded = true;
  }
   clearSfiNavBarStyle() {
    document.body.style.removeProperty('overflow');
  }

  updateRelationship(event) {
  this.updateRelationshipDetails = event;
  }

  addRelationship(event) {
    this.isEnableRelationshipModal = event;
  }

  closedRelationshipModal(event) {
    this.isEnableRelationshipModal = event;
  }

  scrollPosition(event) {
    if (event) {
      scrollIntoView('focusQuestionnaire');
    }
  }
}
