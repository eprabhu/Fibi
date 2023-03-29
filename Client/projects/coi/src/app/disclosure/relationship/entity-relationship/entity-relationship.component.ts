import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';
import { DataStoreService } from '../../services/data-store.service';
import { EntityRelationshipService } from './entity-relationship.service';

@Component({
  selector: 'app-entity-relationship',
  templateUrl: './entity-relationship.component.html',
  styleUrls: ['./entity-relationship.component.css']
})
export class EntityRelationshipComponent implements OnInit {
  isOpenModal: any;
  disclosureStatusCode: any;
  disclosurePersonId: any;
  $subscriptions: Subscription[] = [];
  coiDisclosure: any;

  dependencies = ['coiDisclosure', 'proposalIdlinkedInDisclosure'];

  constructor(public _entityService: EntityRelationshipService,
    private _route: ActivatedRoute,
    private _dataStore: DataStoreService) { }

  ngOnInit() {
    this.getDataFromStore();
    this.listenDataChangeFromStore();
    this.getProjectRelationships();
  }

  getProjectRelationships() {
    this._entityService.getProjectRelationships(
      this._route.snapshot.queryParamMap.get('disclosureId'), this.disclosureStatusCode, this.disclosurePersonId)
      .subscribe((data: any) => {
      this._entityService.projectDetails.next(data);
      if (data.awards.length > 0 && data.proposals.length > 0) {
        this._entityService.commonProjectArray = [...data.proposals, ...data.awards];
      } else if (data.awards && data.proposals.length === 0) {
        this._entityService.commonProjectArray = data.awards;
      } else if (data.awards.length === 0 && data.proposals) {
        this._entityService.commonProjectArray = data.proposals;
      }
      this._entityService.test.next(this._entityService.commonProjectArray);
      this._entityService.$coiStatus.next(data.coiDisclosureDetailStatuses);
    });
  }

	closeModal(event) {
		this.isOpenModal = false;
	}

  notClose(event) {
    let a = this._entityService.commonProjectArray.find(ele => ele.moduleItemId === this._entityService.currentProjectModuleItemKey);
    this._entityService.$setSelectedProjectDetails.next(a);
    this.isOpenModal = false;
  }


private getDataFromStore() {
  const DATA = this._dataStore.getData(this.dependencies);
  this.coiDisclosure = DATA.coiDisclosure;
  this.disclosureStatusCode = DATA.coiDisclosure.disclosureStatusCode;
  this.disclosurePersonId = DATA.coiDisclosure.personId;
  this._entityService.linkedProposalId = DATA.proposalIdlinkedInDisclosure;
}

private listenDataChangeFromStore() {
  this.$subscriptions.push(
      this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
          if (dependencies.some((dep) => this.dependencies.includes(dep))) {
              this.getDataFromStore();
          }
      })
  );
}

  openModal(event) {
		this.isOpenModal = event;
	}

}
