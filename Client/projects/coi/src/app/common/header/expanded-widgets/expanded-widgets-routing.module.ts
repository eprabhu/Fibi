import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ExpandedActionListComponent } from 'projects/fibi/src/app/expanded-widgets/expanded-action-list/expanded-action-list.component';


const routes: Routes = [
  {
    path: 'expanded-widgets', component: ExpandedActionListComponent,
  }];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
  providers: []
})

export class ExpandedWidgetsRoutingModule { }
