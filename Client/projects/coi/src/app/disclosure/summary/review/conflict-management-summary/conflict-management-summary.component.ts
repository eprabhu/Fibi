import { Component } from '@angular/core';
import {CoiSummaryEventsAndStoreService} from "../../coi-summary-events-and-store.service";
import {CommentConfiguration} from "../../../coi-interface";

@Component({
  selector: 'app-conflict-management-summary',
  templateUrl: './conflict-management-summary.component.html',
  styleUrls: ['./conflict-management-summary.component.scss']
})
export class ConflictManagementSummaryComponent {

  commentConfiguration: CommentConfiguration = new CommentConfiguration();

  constructor(private _dataStoreAndEventsService: CoiSummaryEventsAndStoreService) {
  }

  modifyReviewComment() {
    this._dataStoreAndEventsService.modifyReviewComment(this.commentConfiguration);
  }

}
