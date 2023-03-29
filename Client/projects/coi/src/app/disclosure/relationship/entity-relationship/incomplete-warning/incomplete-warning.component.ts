import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { CommonService } from '../../../../common/services/common.service';
import { DataStoreService } from '../../../services/data-store.service';
import { EntityRelationshipService } from '../entity-relationship.service';

declare var $: any;

@Component({
  selector: 'app-incomplete-warning',
  templateUrl: './incomplete-warning.component.html',
  styleUrls: ['./incomplete-warning.component.css']
})
export class IncompleteWarningComponent implements OnInit {

  @Input() currentProject: any = {};
  @Input() nextProject: any = {};
  @Input() nextProjectModuleItemKey: any = {};
  @Input() nextProjectPosition: 'P' | 'N' | 'O';
  @Output() closeModal = new EventEmitter<any>();
  @Output() notClose = new EventEmitter<any>();

  constructor(
    public _entityService: EntityRelationshipService,
    private _dataStore: DataStoreService,
    public commonService: CommonService,
    private _route: ActivatedRoute) { }

  ngOnInit() {
    $('#incompleteWarning').modal('show');
  }

  setPreviousNext() {
    this.closeModal.emit(false);
    if (this.nextProjectPosition === 'O') {
      const proposal = this._entityService.commonProjectArray.find(ele => ele.moduleItemId === this._entityService.nextProjectModuleItemKey);
      this._entityService.$setSelectedProjectDetails.next(proposal);
		} else {
      this._entityService.$previousNext.next(this.nextProjectPosition);
    }
  }

  closeEmitModal(type) {
    this.notClose.emit(true);
  }

}
