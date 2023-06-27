import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { EntityDetailsService } from './entity-details.service';
import { scrollIntoView } from '../../../../../fibi/src/app/common/utilities/custom-utilities';

@Component({
  selector: 'app-entity-details',
  templateUrl: './entity-details.component.html',
  styleUrls: ['./entity-details.component.scss']
})
export class EntityDetailsComponent implements  OnInit, OnDestroy {
  @Input() entityId: any;
  isTriggeredFromSlider = false;

  constructor(public entityDetailService: EntityDetailsService, private _route: ActivatedRoute, private _router: Router) {
    this.clearSfiNavBarStyle();
  }
  updateRelationshipDetails: any;
  isEnableRelationshipModal = false;
  sfiRelationStatus: any = {};
  isEditMode = false;

  ngOnInit() {
    this.isEditMode = this._route.snapshot.queryParamMap.get('mode') === 'edit';
    this.entityId = this._route.snapshot.queryParamMap.get('personEntityId') || this.entityId;
    this.isTriggeredFromSlider = this._router.url.includes('create-disclosure') ? true : false;
    if (this.isEditMode) {
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

  getRelationshipDetails(event) {
    this.sfiRelationStatus = event;
  }
}
