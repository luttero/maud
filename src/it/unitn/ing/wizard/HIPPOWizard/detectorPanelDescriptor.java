package it.unitn.ing.wizard.HIPPOWizard;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import it.unitn.ing.wizard.WizardPanelDescriptor;

public class detectorPanelDescriptor extends WizardPanelDescriptor implements ActionListener {

	public static final String IDENTIFIER = "Detectors";
	DetectorPanel panel;

    public detectorPanelDescriptor(HIPPOdata data) {
      panel = new DetectorPanel(data);
    	panel.addActionListener(this);
    	setPanelDescriptorIdentifier(IDENTIFIER);
      setPanelComponent(panel);
    }
    
    public Object getNextPanelDescriptor() {
        return FINISH;
    }
    
    public Object getBackPanelDescriptor() {
        return dataFilesPanelDescriptor.IDENTIFIER;
    }
    
    public void aboutToDisplayPanel() {
    	checkPanelData();
    }
    
    public void aboutToHidePanel() {
    	panel.saveData();
    }  

	public void actionPerformed(ActionEvent e) {
		checkPanelData();
	}
	
	private void checkPanelData() {
		if (panel.enabledToContinue())
      getWizard().setNextFinishButtonEnabled(true);
		else
      getWizard().setNextFinishButtonEnabled(false);
	}

}
