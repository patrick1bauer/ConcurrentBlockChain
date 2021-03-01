import java.math.BigInteger;
import java.security.MessageDigest;

public class BlockchainSingleThreaded
{

    public String calcHash(String input)
    {
        byte[] byteRep = null;
        String hashed = null;
        try {
            // Get MessageDigest object which implements sha 256
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            // Get byte array of hash
            byteRep = md.digest(input.getBytes());
            // Convert byte array to correctly formatted string
            hashed = String.format("%x", new BigInteger(1, byteRep));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return hashed;
    }

    public static void main(String[] args)
    {

    }
}