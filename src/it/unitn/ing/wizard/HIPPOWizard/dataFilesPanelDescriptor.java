package it.unitn.ing.wizard.HIPPOWizard;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import it.unitn.ing.wizard.WizardPanelDescriptor;

public class dataFilesPanelDescriptor extends WizardPanelDescriptor implements PropertyChangeListener  {
	
	public static final String IDENTIFIER = "Data files";
	private dataFilesPanel panel;

    public dataFilesPanelDescriptor(HIPPOdata data) {
    	panel = new dataFilesPanel(data);
    	panel.addPropertyChangeListener(this);
    	setPanelDescriptorIdentifier(IDENTIFIER);
        setPanelComponent(panel);
    }
    
    public Object getNextPanelDescriptor() {
        return detectorPanelDescriptor.IDENTIFIER;
    }
    
    public Object getBackPanelDescriptor() {
        return null;
    }
    
    public void aboutToDisplayPanel() {
    	checkPanelData();
    }
    
    public void aboutToHidePanel() {
    	panel.saveData();
    }
	
	public void propertyChange(PropertyChangeEvent evt) {
		checkPanelData();
	}
	
	private void checkPanelData() {
		if (panel.enabledToContinue())
            getWizard().setNextFinishButtonEnabled(true);
		else
            getWizard().setNextFinishButtonEnabled(false);
	}
	

}
